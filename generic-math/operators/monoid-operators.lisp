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
         (t (call-next-method (apply (operator expression) new-operands))))))))

(defgeneric identity-value (object))

(defparameter *monoid-binary-format* "~a-gbin")

(defmacro defm-monoid-genop (gen-name identity-value
                             &key
                               (symbolic-name gen-name)
                               (binary-name (format-symbol *monoid-binary-format* symbolic-name))
                               (docstring '())
                               (expression-superclass 'monoid-expression)
                               (exp-class-name (format-symbol *exp-class-format* symbolic-name))
                               (exp-maker-name (format-symbol *exp-maker-format* symbolic-name))
                               (extra-exp-slots '())
                               (sorter #'identity)
                               (merger #'nconc))
  (eval-once (identity-value)
    `(progn
       (defun ,gen-name (&rest operands)
         (if (null (cdr operands))
             (car operands)
             (reduce #',binary-name (funcall ,sorter operands))))
          
       (defmgenop ,binary-name (x y)
                  :symbolic-name ,symbolic-name
                  :operator-name ,gen-name
                  :docstring ,docstring
                  :expression-superclass ,expression-superclass
                  :exp-class-name ,exp-class-name
                  :exp-maker nil
                  :extra-exp-slots ((identity-value :allocation :class
                                                    :reader identity-value
                                                    :initform ,identity-value)
                                    ,@extra-exp-slots))

       (defun ,exp-maker-name (&rest operands)
         (make-instance ',exp-class-name :operands operands))

       (defmethod ,binary-name ((x ,exp-class-name) (y ,exp-class-name))
         (reduce #',binary-name (funcall ,merger (operands x) (operands y))))

       (defmethod ,binary-name ((x ,exp-class-name) y)
         (let ((last-xop (car (last (operands x)))))
           (if (g/= (funcall ,sorter (list last-xop y))
                    (list last-xop y))
               (let ((tmp (,binary-name last-xop y)))
                 (if (g/= tmp (,exp-maker-name last-xop y))
                     (apply #',exp-maker-name (append (operands x) (list y)))
                     (reduce #',binary-name (funcall ,merger (butlast (operands x)) (list tmp)))))
               (,binary-name y x))))

       (defmethod ,binary-name (x (y ,exp-class-name))
         (reduce #',binary-name (funcall ,merger (list x) (operands y))))

       (defmethod ,binary-name (x (y expression))
         (,exp-maker-name x y))

       (defmethod identity-value ((symbol (eql ',symbolic-name))) ,identity-value))))
