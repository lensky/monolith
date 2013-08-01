(in-package #:monolith)

(defclass abelian-expression (monoid-expression) ()
  (:documentation "Class for commutative monoidal operators with an inverse element."))

(defgeneric abelian< (x y)
  (:documentation "Used to sort arguments to abelian operators."))

(defmethod abelian< (x y) nil)

(defmethod abelian< ((x number) y) t)
(defmethod abelian< (x (y number)) nil)

(defun abelian<= (x y)
  (or (abelian< x y)
      (not (abelian< y x))))

(defun sort-abelian-operands (operands)
  (stable-sort operands #'abelian<))

(defun merge-abelian-operands (op1 op2)
  (merge 'list op1 op2 #'abelian<))

(defmacro defm-abelian-genop (gen-name identity-value
                              &key
                                (symbolic-name gen-name)
                                (binary-name (format-symbol *monoid-binary-format* symbolic-name))
                                (docstring '())
                                (expression-superclass 'abelian-expression)
                                (exp-class-name (format-symbol *exp-class-format* symbolic-name))
                                (exp-maker-name (format-symbol *exp-maker-format* symbolic-name))
                                (extra-exp-slots '()))
  (eval-once (identity-value)
    `(defm-monoid-genop ,gen-name ,identity-value
       :symbolic-name ,symbolic-name
       :binary-name ,binary-name
       :docstring ,docstring
       :expression-superclass ,expression-superclass
       :exp-class-name ,exp-class-name
       :exp-maker-name ,exp-maker-name
       :extra-exp-slots ((inverse-fn :allocation :class
                                     :reader identity-value
                                     :initform ,identity-value)
                         ,@extra-exp-slots)
       :sorter #'sort-abelian-operands
       :merger #'merge-abelian-operands)))

(defmacro defm-abelian-inverse-genop (gen-name inverse-name numeric-fn mult-fn
                                      &key
                                        (symbolic-name gen-name)
                                        (binary-name (format-symbol *monoid-binary-format* symbolic-name))
                                        (docstring '())
                                        (expression-superclass 'expression)
                                        (exp-class-name (format-symbol *exp-class-format* symbolic-name))
                                        (exp-maker-name (format-symbol *exp-maker-format* symbolic-name))
                                        (extra-exp-slots '())
                                        (sorter 'sort-abelian-operands)
                                        (merger 'merge-abelian-operands))
  (let ((inverse-class-name (expression-class inverse-name)))
   `(let ((id-val (identity-value ',inverse-name)))
      (defun ,gen-name (&rest operands)
        (if (null (cdr operands))
            (if (numberp (car operands))
                (funcall ,numeric-fn (car operands))
                (funcall ,mult-fn (car operands) -1))
            (reduce #',binary-name (cons (car operands) (,sorter (cdr operands))))))

      (defmgenop ,binary-name (x y)
        :symbolic-name ,symbolic-name
        :operator-name ,gen-name
        :docstring ,docstring
        :expression-superclass ,expression-superclass
        :exp-class-name ,exp-class-name
        :exp-maker nil
        :extra-exp-slots ((inverse-name :allocation :class
                                        :reader inverse-name
                                        :initform nil)
                          ,@extra-exp-slots))

      (defun ,exp-maker-name (&rest operands)
        (make-instance ',exp-class-name :operands operands))

      (defmethod ,binary-name ((x number) (y number))
        (funcall ,numeric-fn x y))

      (defmethod ,binary-name ((x ,exp-class-name) (y ,inverse-class-name))
        (reduce #',binary-name (cons (car (operands x)) (,merger (cdr (operands x)) (operands y)))))

      (defmethod ,binary-name (x y)
        (,inverse-name x (funcall ,mult-fn y -1))))))
