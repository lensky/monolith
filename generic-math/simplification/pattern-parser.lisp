(in-package #:monolith)

(defcombinator c-mexpr (operator-name operands-combinator)
    (input state succeed fail)
  (if (or (not (subtypep (type-of input) 'expression))
          (not (eq operator-name (operator-name input))))
      (fail)
      (let ((result (call-combinator operands-combinator (operands input) state)))
        (succeed (value result) (remainder result) (state result)))))

(defun expr-pat (operator-name operand-combinators)
  (with-id-success (c-mexpr operator-name (apply #'make-list-pat operand-combinators))))

(defun listing-var-p (x)
  (and (symbolp x)
       (eq (char (symbol-name x) 0) #\@)))

(defun atomic-var-p (x)
  (and (symbolp x)
       (eq (char (symbol-name x) 0) #\?)))

(defun literal-p (x)
  (numberp x))

(defparameter *listing-var-comb*
  (funcall
   (c-predicate #'listing-var-p (with-id-success c-list-terminal))
   (lambda (v s)
     (declare (ignore s))
     (make-listing-var v))))

(defparameter *atomic-single-var-comb*
  (funcall
   (c-predicate #'atomic-var-p (with-id-success c-list-terminal))
   (lambda (v s)
     (declare (ignore s))
     (make-atomic-single-var v))))

(defparameter *literal-comb*
  (funcall
   (c-predicate #'literal-p (with-id-success c-list-terminal))
   (lambda (v s)
     (declare (ignore s))
     (make-literal-var v))))

;; ADD RECURSION
(defparameter *sub-expr-comb*
  (funcall
   (c-sublist
    (with-id-success
        (c-list
         (mapcar #'with-id-success
                 (list
                  c-list-terminal
                  (c-star
                   (with-id-success
                       (c-or
                        (list
                         *literal-comb*
                         *atomic-single-var-comb*
                         *listing-var-comb*
                         'sub-expr-comb))))))))
    :recursive-name 'sub-expr-comb)
   (lambda (lst s)
     (declare (ignore s))
     (let+ (((&ign expr-name combinators &ign) lst))
       (with-id-success
           (c-sublist (expr-pat expr-name combinators)))))))

(defparameter *expr-comb*
  (funcall
   (c-list
    (list
     (with-id-success
         (c-star
          (with-id-success
              (c-or
               (list
                *literal-comb*
                *atomic-single-var-comb*
                *listing-var-comb*
                *sub-expr-comb*)))))))
   (lambda (lst s)
     (declare (ignore s))
     (let+ (((&ign combinators &ign) lst))
       (c-list combinators)))))

(defun pattern->comb (pattern)
  (first-value *expr-comb* pattern '()))
