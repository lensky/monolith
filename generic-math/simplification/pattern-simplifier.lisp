(in-package #:monolith)

(defclass c-m-expr (combinator)
  ((operator-name :reader operator-name
                  :initarg :operator-name)
   (operands-combinators :reader operands-combinators
                     :initarg :operands-combinators)))

(defun exp-pat (operator-name operand-combs)
  (make-instance 'c-m-expr
                 :succeed *id-succeed*
                 :fail (make-simple-fail 'r '`(format nil "Failed to match expr: ~a." ,r))
                 :operator-name operator-name
                 :operands-combinators operand-combs))

(def-comb->lisp (c-m-expr input state succeed fail
                          :vars (fail-immediately c-gen c-res))
  (let ((operands-comb (compile-combinator (apply #'make-list-pat (operands-combinators c-m-expr))))
        (operator-name (operator-name c-m-expr)))
    `(let ((,fail-immediately (or (not (subtypep (type-of ,input) 'expression))
                                  (not (eq ',operator-name (operator-name ,input))))))
       (let ((,c-gen (unless ,fail-immediately
                       (funcall ,operands-comb (operands ,input) ,state))))
         (lambda ()
           (if (not ,fail-immediately)
               (let ((,c-res (funcall ,c-gen)))
                 (if (c-success-p ,c-res)
                     (,succeed (value ,c-res) (remainder ,c-res) (state ,c-res))
                     (,fail ,c-res ,state)))
               (,fail ,input ,state)))))))

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

(defun surround-with-match-env (bindings &rest body)
  (let ((syms-to-bind (mappend (lambda (s) (get-simp-pattern-vars s)) body)))
    (if syms-to-bind
        (with-gensyms (bs)
          `(let ((,bs ,bindings))
             (let (,@(mapcar (lambda (symbol)
                               (let ((var symbol))
                                 `(,symbol (cdr (assoc ',var ,bs)))))
                             syms-to-bind))
               ,@body)))
        `(progn ,@body))))

(defun compile-simplifier-pat (symbolic-pat)
  (let ((sym-mod (cons (gensym) symbolic-pat)))
   (compile-combinator
    (apply #'make-list-pat
           (operands-combinators (pattern->comb sym-mod))))))

(defun single-pattern-simplifier (simp-spec auto-simplify)
  (destructuring-bind (pat action &key (auto-simplify auto-simplify)) simp-spec
    (let ((compiled-pat (compile-simplifier-pat pat)))
      (with-gensyms (operands match new-exp)
        `(lambda (,operands)
           (let ((,match (funcall (funcall ,compiled-pat ,operands '()))))
             (if (c-failure-p ,match)
                 (cons ,operands nil)
                 (let ((,new-exp ,(surround-with-match-env `(state ,match) action)))
                   (cons ,(if auto-simplify `(simplify-exp ,new-exp) new-exp) t)))))))))

(defun make-pm-simplifier (op-fn auto-simplify simp-specs)
  (compile nil
           (eval
            (with-gensyms (compiled-simps x fn-list fn res matched-p new-operands)
              `(let ((,compiled-simps
                      (mapcar #'(lambda (,x) (compile nil (single-pattern-simplifier ,x ,auto-simplify)))
                              ',simp-specs)))
                 (lambda (operands)
                   (let ((,matched-p nil)
                         (,new-operands operands))
                     (do* ((,fn-list ,compiled-simps (cdr ,fn-list))
                           (,fn (car ,fn-list) (car ,fn-list)))
                          ((or (null ,fn) ,matched-p)
                           (if ,matched-p
                               ,new-operands (apply ,op-fn ,new-operands)))
                       (let ((,res (funcall ,fn ,new-operands)))
                         (setf ,matched-p (cdr ,res))
                         (setf ,new-operands (car ,res)))))))))))

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
           (let ((compiled-simplifier
                  (make-pm-simplifier (symbol-function expr-sym)
                                      t
                                      (gethash expr-sym *expr-pattern-specs*))))
             (setf (gethash expr-sym *expr-compiled-pattern-simplifiers*) compiled-simplifier)))))

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
