(in-package #:monolith)

(defgeneric simplify-exp (expression)
  (:documentation "Implement for expression type for simplification."))

(defmethod simplify-exp (x) x)

(defmethod simplify-exp :around ((expression expression))
  (let ((new-expression (apply (operator expression)
                               (mapcar #'simplify-exp (operands expression)))))
    (if (eq (class-of new-expression)
            (class-of expression))
        (call-next-method new-expression)
        (simplify-exp new-expression))))
