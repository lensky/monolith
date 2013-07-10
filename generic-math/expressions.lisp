(in-package #:monolith)

(defclass expression ()
  ((operands :accessor operands
             :initarg :operands
             :type list
             :initform (error "Must supply operands"))))

(defun expressionp (object)
  (typep object 'expression))

(defmethod g/=-bin ((x expression) (y expression))
  (and (g/= (operator x) (operator y))
       (g/= (operands x) (operands y))))

(defmethod print-object ((exp expression) stream)
  (format stream "(~a ~{~a~^ ~})" (operator-name exp) (operands exp)))
