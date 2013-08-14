(in-package #:monolith)

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
