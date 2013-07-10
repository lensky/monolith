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

(defun surround-with-pat-env (symbolic-pat &rest body)
  `(let (,@(mapcar #'pattern-symbol->let-statement
                   (get-simp-pattern-vars symbolic-pat)))
     ,@body))

(defun surround-with-match-env (symbolic-pat bindings-sym &rest body)
  `(let (,@(mapcar (lambda (symbol)
                     (let ((var (eval (pattern-symbol->pattern-var symbol))))
                       `(,symbol ,(compile-get-binding-val var bindings-sym))))
                   (get-simp-pattern-vars symbolic-pat)))
     ,@body))

(defun compile-simplifier-pat (symbolic-pat)
  (eval
   (labels ((add-exp-pat (x)
              (if (listp x)
                  (append `(exp-pat ',(car x)) (mapcar #'add-exp-pat (cdr x)))
                  x)))
     (let ((with-exp-pat (mapcar #'add-exp-pat symbolic-pat)))
       (surround-with-pat-env symbolic-pat `(compile-pattern (list-pat ',(gensym) ,@with-exp-pat)))))))

(defun single-pattern-simplifier (simp-spec auto-simplify)
  (destructuring-bind (pat action &key (auto-simplify auto-simplify)) simp-spec
    (let ((compiled-pat (compile-simplifier-pat pat)))
      (with-gensyms (operands match bindings new-exp)
        `(lambda (,operands)
           (let ((,match (funcall (funcall ,compiled-pat (single-value-matcher (list ,operands))))))
             (if (pattern-false-p ,match)
                 (cons ,operands nil)
                 (let* ((,bindings (cdr ,match))
                        (,new-exp ,(surround-with-match-env pat bindings action)))
                   (cons ,(if auto-simplify `(simplify-exp ,new-exp) new-exp) t)))))))))

(defun make-pm-simplifier (op-fn before-simplifier after-simplifier auto-simplify simp-specs)
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

(defun set-pm-simplifier (expression-sym simp-specs &optional before-simplifier after-simplifier (auto-simplify t))
  (let* ((expression-type (expression-class expression-sym))
         (compiled-simplifier (make-pm-simplifier (symbol-function expression-sym)
                                before-simplifier
                                after-simplifier
                                auto-simplify
                                simp-specs)))
    (eval `(defmethod simplify-exp ((expression ,expression-type))
             (funcall ,compiled-simplifier (operands expression))))))

(defvar *expr-pattern-specs* (make-hash-table))

(defmacro add-simplifier-patterns (&body simp-specs)
  (let ((setting-required
         (loop for simp-spec in simp-specs
            do (let ((expr-sym (caar simp-spec))
                     (spec (list (cdar simp-spec) (cadr simp-spec))))
                 (setf (gethash expr-sym *expr-pattern-specs*)
                       (remove-duplicates
                        (append (gethash expr-sym *expr-pattern-specs* '())
                                (list spec)))))
            collect (caar simp-spec))))
    `(progn
       ,@(loop for expr-sym in setting-required
              collect `(set-pm-simplifier ',expr-sym
                         (gethash ',expr-sym *expr-pattern-specs*))))))
