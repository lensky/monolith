(in-package #:monolith)

(defclass monoid-expression (expression) ()
  (:documentation "Class for associative operators with an identity element."))

(defmethod simplify-exp :around ((expression monoid-expression))
  (let ((identity-value (identity-value expression)))
   (labels ((remove-identity (operands)
              (remove-if (lambda (x) (g/= x identity-value)) operands)))
     (let ((new-operands (remove-identity (operands expression))))
       (cond
         ((null new-operands) identity-value)
         ((null (cdr new-operands)) (car new-operands))
         (t (setf (operands expression) new-operands)
            (call-next-method expression)))))))

(defgeneric identity-value (object))

(defmacro defm-monoid-genop (gen-name args identity-value
                             &key
                               (symbolic-name gen-name)
                               (operator-name gen-name)
                               (docstring '())
                               (expression-superclass 'monoid-expression)
                               (exp-class-name (format-symbol *exp-class-format* symbolic-name))
                               (exp-maker t)
                               (exp-maker-name (format-symbol *exp-maker-format* symbolic-name))
                               (extra-exp-slots '()))
  (eval-once (identity-value)
    `(progn
       (defmgenop ,gen-name ,args
         :symbolic-name ,symbolic-name
         :operator-name ,operator-name
         :docstring ,docstring
         :expression-superclass ,expression-superclass
         :exp-class-name ,exp-class-name
         :exp-maker ,exp-maker
         :exp-maker-name ,exp-maker-name
         :extra-exp-slots ((identity-value :allocation :class
                                           :reader identity-value
                                           :initform ,identity-value)
                           ,@extra-exp-slots))

       (defmethod identity-value ((symbol (eql ',symbolic-name))) ,identity-value))))
