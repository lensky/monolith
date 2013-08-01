(in-package #:monolith)

(defclass m-expression-pat ()
  ((operator-name :reader operator-name
                  :initarg :operator-name)
   (operands-matcher :reader operands-matcher
                     :initarg :operands-matcher)))

(defun exp-pat (operator-name &rest operand-pats)
  (make-instance 'm-expression-pat
                 :operator-name operator-name
                 :operands-matcher
                 (compile-pattern (apply #'list-pat (gensym) operand-pats))))

(defmethod compile-pattern ((pattern m-expression-pat))
  (make-pattern-compiler (pattern unmatched bindings fail
                          :state-vars (op-match-gen))
      (null op-match-gen)
    (t
     `(cond
        (op-match-gen
         (let ((op-res (funcall op-match-gen)))
           (if (pattern-false-p op-res)
               (progn
                 (setf op-match-gen nil)
                 (fail))
               (cons (cdr unmatched) (cdr op-res)))))
        ((< (length unmatched) 1)
         (setf op-match-gen nil)
         (fail))
        (t
         (let ((expression (car unmatched)))
           (if (and (expressionp expression)
                    (eq ',(operator-name pattern) (operator-name expression)))
               (setf op-match-gen
                     (funcall ,(operands-matcher pattern)
                              (single-value-matcher (list (operands expression))
                                                    bindings)))
               (setf op-match-gen nil))
           (fail)))))))

(defparameter *pattern-prefix->object*
  `((#\? . ,(lambda (name) `(atomic ',name :single-valued t)))
    (#\@ . ,(lambda (name) `(listing ',name :single-valued nil)))
    (#\$ . ,(lambda (name) `(listing ',name :single-valued t))))
  "List linking prefixes to appropiate constructors.")

(defun tree-find (tree predicate)
  (if (listp tree)
      (mappend (lambda (x) (tree-find x predicate)) tree)
      (if (funcall predicate tree) (list tree) nil)))

(defun pattern-symbol->pattern-var (symbol)
  (funcall (cdr
            (assoc
             (char (symbol-name symbol) 0)
             *pattern-prefix->object*))
           symbol))

(defun pattern-symbol->let-statement (symbol)
  (list symbol (pattern-symbol->pattern-var symbol)))

(defun get-simp-pattern-vars (symbolic-pat)
  (remove-duplicates
   (tree-find symbolic-pat
              (lambda (x)
                (and
                 (symbolp x)
                 (member (char (symbol-name x) 0)
                         (mapcar #'car *pattern-prefix->object*)))))))

(defun surround-with-pat-env (&rest body)
  `(let (,@(mapcar #'pattern-symbol->let-statement
                   (mappend (lambda (s) (get-simp-pattern-vars s)) body)))
     ,@body))

(defun surround-with-match-env (bindings &rest body)
  (let ((syms-to-bind (mappend (lambda (s) (get-simp-pattern-vars s)) body)))
    (if syms-to-bind
        (with-gensyms (bs)
          `(let ((,bs ,bindings))
             (let (,@(mapcar (lambda (symbol)
                               (let ((var (eval (pattern-symbol->pattern-var symbol))))
                                 `(,symbol ,(compile-get-binding-val var bs))))
                             syms-to-bind))
               ,@body)))
        `(progn ,@body))))

(defun compile-simplifier-pat (symbolic-pat)
  (eval
   (labels ((add-exp-pat (x)
              (if (listp x)
                  (append `(exp-pat ',(car x)) (mapcar #'add-exp-pat (cdr x)))
                  x)))
     (let ((with-exp-pat (mapcar #'add-exp-pat symbolic-pat)))
       (surround-with-pat-env `(compile-pattern (list-pat ',(gensym) ,@with-exp-pat)))))))

(defun single-pattern-simplifier (simp-spec auto-simplify)
  (destructuring-bind (pat action &key (auto-simplify auto-simplify)) simp-spec
    (let ((compiled-pat (compile-simplifier-pat pat)))
      (with-gensyms (operands match new-exp)
        `(lambda (,operands)
           (let ((,match (funcall (funcall ,compiled-pat (single-value-matcher (list ,operands))))))
             (if (pattern-false-p ,match)
                 (cons ,operands nil)
                 (let ((,new-exp ,(surround-with-match-env `(cdr ,match) action)))
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
                         (,res nil)
                         (,new-operands operands))
                     (do* ((,fn-list ,compiled-simps (cdr ,fn-list))
                           (,fn (car ,fn-list) (car ,fn-list)))
                          ((or (null ,fn) ,matched-p)
                           (if ,matched-p
                               ,new-operands (apply ,op-fn ,new-operands)))
                       (setf ,res (funcall ,fn ,new-operands))
                       (setf ,matched-p (cdr ,res))
                       (setf ,new-operands (car ,res))))))))))

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
           (let ((expression-type (expression-class expr-sym)))
             (let ((compiled-simplifier
                    (make-pm-simplifier (symbol-function expr-sym)
                                        t
                                        (gethash expr-sym *expr-pattern-specs*))))
               (setf (gethash expr-sym *expr-compiled-pattern-simplifiers*) compiled-simplifier))))))

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
