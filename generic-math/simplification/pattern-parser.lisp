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

(defun listing-var-p (x)
  (and (symbolp x)
       (eq (char (symbol-name x) 0) #\@)))

(defun atomic-var-p (x)
  (and (symbolp x)
       (eq (char (symbol-name x) 0) #\?)))

(defun literal-p (x)
  (numberp x))

(defparameter *listing-var-comb*
  (make-instance 'c-predicate
                 :predicate #'listing-var-p
                 :combinator *basic-list-terminal*
                 :succeed (lambda (v r s)
                            (declare (ignore r s))
                            (make-listing-var v))
                 :fail *id-fail*))

(defparameter *atomic-single-var-comb*
  (make-instance 'c-predicate
                 :predicate #'atomic-var-p
                 :combinator *basic-list-terminal*
                 :succeed (lambda (v r s)
                            (declare (ignore r s))
                            (make-atomic-single-var v))
                 :fail *id-fail*))

(defparameter *literal-comb*
  (make-instance 'c-predicate
                 :predicate #'literal-p
                 :combinator *basic-list-terminal*
                 :succeed (lambda (v r s)
                            (declare (ignore r s))
                            (make-literal-var v))
                 :fail *id-fail*))

(defparameter *sub-expr-comb*
  (make-sublist-comb '()
                     (lambda (lst remainder state)
                       (declare (ignore remainder state))
                       (let+ (((&ign expr-name combinators &ign) lst))
                         (make-list-term-comb-comb
                          (exp-pat expr-name combinators)
                          *id-succeed*
                          *id-fail*)))
                     *id-fail*))

(setf (slot-value *sub-expr-comb* 'combinators)
      (list *basic-list-terminal*
            (make-star-comb
             (make-or-comb
              (list *literal-comb*
                    *atomic-single-var-comb*
                    *listing-var-comb*
                    *sub-expr-comb*)
              *id-succeed*
              *id-fail*)
             *id-succeed*
             *id-fail*)))

(defparameter *expr-comb*
  (make-list-comb (list *basic-list-terminal*
                        (make-star-comb
                         (make-or-comb
                          (list *literal-comb*
                                *atomic-single-var-comb*
                                *listing-var-comb*
                                *sub-expr-comb*)
                          *id-succeed*
                          *id-fail*)
                         *id-succeed*
                         *id-fail*))
                  (lambda (lst remainder state)
                    (declare (ignore remainder state))
                    (let+ (((&ign expr-name combinators &ign) lst))
                      (exp-pat expr-name combinators)))
                  *id-fail*))

(defparameter *simp-pattern-parser* (compile-combinator *expr-comb*))

(defun pattern->comb (pattern)
  (let ((result (funcall (funcall *simp-pattern-parser* pattern '()))))
    (if (c-success-p result)
        (value result)
        result)))
