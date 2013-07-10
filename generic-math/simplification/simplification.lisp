(in-package #:monolith)

(defgeneric simplify-exp (expression)
  (:documentation "Implement for expression type for simplification."))

(defmethod simplify-exp (x) x)

(defmethod simplify-exp :around ((exp expression))
  (setf (operands exp) (mapcar #'simplify-exp (operands exp)))
  (call-next-method exp))
