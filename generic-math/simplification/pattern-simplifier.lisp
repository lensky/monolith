(in-package #:monolith)

(defun tree-find (tree predicate)
  (if (listp tree)
      (mappend (lambda (x) (tree-find x predicate)) tree)
      (if (funcall predicate tree) (list tree) nil)))

(defun get-simp-pattern-vars (symbolic-pat)
  (remove-duplicates
   (tree-find symbolic-pat
              (lambda (x)
                (or (listing-var-p x)
                    (atomic-var-p x))))))

(defun compile-simplifier-pat (symbolic-pat action auto-simplify)
  (funcall (pattern->comb symbolic-pat)
           (let ((syms-to-bind (get-simp-pattern-vars action))
                 (action-exp (if auto-simplify
                                 `(simplify-exp ,action)
                                 action)))
             (compile
              nil
              (with-gensyms (v s)
                `(lambda (,v ,s)
                   ,@(if syms-to-bind
                         `((declare (ignore ,v))
                           (let (,@(mapcar
                                    (lambda (symbol)
                                      `(,symbol (cdr (assoc ',symbol ,s))))
                                    syms-to-bind))
                             ,action-exp))
                         `((declare (ignore ,v ,s))
                           ,action-exp))))))))

(defun single-pattern-simplifier (simp-spec auto-simplify)
  (destructuring-bind (pat action &key (auto-simplify auto-simplify)) simp-spec
    (let ((compiled-pat (compile-simplifier-pat pat action auto-simplify)))
      (lambda (operands)
        (first-value compiled-pat operands '())))))

(defun make-pm-simplifier (op-fn simp-specs auto-simplify)
  (let ((compiled-simps
         (mapcar #'(lambda (x)
                     (single-pattern-simplifier x auto-simplify))
                 simp-specs)))
    (lambda (operands)
      (or
       (iter (for simp in compiled-simps)
             (for match = (funcall simp operands))
             (if match (leave match)))
       (apply op-fn operands)))))

(defvar *expr-pattern-specs* (make-hash-table))
(defvar *expr-compiled-pattern-simplifiers* (make-hash-table))

(defun pattern-simplifier (expression)
  (gethash (operator-name expression) *expr-compiled-pattern-simplifiers* nil))

(defun add-patterns-to-table (hash-table specs)
  (iter (for ((expr-sym . pat) . action-spec) in specs)
        (let ((present (assoc pat (gethash expr-sym hash-table) :test #'equalp)))
          (if present
              (setf (cdr present) action-spec)
              (setf (gethash expr-sym hash-table)
                    (append (gethash expr-sym hash-table '())
                            (list (cons pat action-spec))))))
        (adjoining expr-sym)))

(defmacro add-simplifier-patterns (&body simp-specs)
  `(let ((recompile-required
          (add-patterns-to-table *expr-pattern-specs* ',simp-specs)))
     (iter (for expr-sym in recompile-required)
           (let ((precompiled-simplifier
                  (make-pm-simplifier (symbol-function expr-sym)
                                      (gethash expr-sym *expr-pattern-specs*)
                                      t)))
             (setf (gethash expr-sym *expr-compiled-pattern-simplifiers*) (compile nil precompiled-simplifier))))))

(defmethod simplify-exp :around ((expression expression))
  (let ((new-expression
         (apply (operator expression)
                (mapcar #'simplify-exp (operands expression)))))
    (if (eq (class-of new-expression)
            (class-of expression))
        (let* ((simplified (call-next-method new-expression))
               (pattern-simplifier (pattern-simplifier simplified)))
          (if pattern-simplifier
              (funcall pattern-simplifier (operands simplified))
              simplified))
        (simplify-exp new-expression))))
