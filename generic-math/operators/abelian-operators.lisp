(in-package #:monolith)

(defclass abelian-expression (monoid-expression) ()
  (:documentation "Class for commutative monoidal operators with an inverse element."))

(defgeneric abelian<= (x y)
  (:documentation "Used to sort arguments to abelian operators."))

(defmethod abelian<= (x y) t)

(defmethod abelian<= ((x number) y) t)
(defmethod abelian<= (x (y number)) nil)

(defmethod abelian<= ((x expression) (y expression))
  (string-not-greaterp (symbol-name (operator-name x))
                       (symbol-name (operator-name y))))
(defmethod abelian<= ((x expression) y) nil)
(defmethod abelian<= (x (y expression)) t)

(defmethod abelian<= ((x symbol) (y symbol))
  (string-not-greaterp (symbol-name x) (symbol-name y)))

(defun sort-abelian-operands (operands)
  (sort operands #'abelian<=))

(defun merge-abelian-operands (op1 op2)
  (merge 'list op1 op2 #'abelian<=))

(defparameter *abelian-binary-format* "~a-gbin")

(defmacro defm-abelian-genop (gen-name identity-value
                              &key
                                (symbolic-name gen-name)
                                (binary-name (format-symbol *abelian-binary-format* symbolic-name))
                                (docstring '())
                                (expression-superclass 'abelian-expression)
                                (exp-class-name (format-symbol *exp-class-format* symbolic-name))
                                (exp-maker-name (format-symbol *exp-maker-format* symbolic-name))
                                (extra-exp-slots '())
                                (sorter 'sort-abelian-operands)
                                (merger 'merge-abelian-operands))
  (eval-once (identity-value)
    `(progn
       (defun ,gen-name (&rest operands)
         (if (null (cdr operands))
             (car operands)
             (reduce #',binary-name (,sorter operands))))
          
       (defm-monoid-genop ,binary-name (x y) ,identity-value
                          :symbolic-name ,symbolic-name
                          :operator-name ,gen-name
                          :docstring ,docstring
                          :expression-superclass ,expression-superclass
                          :exp-class-name ,exp-class-name
                          :exp-maker nil
                          :extra-exp-slots ((inverse-fn :allocation :class
                                                        :reader inverse-fn
                                                        :initform nil)
                                            ,@extra-exp-slots))

       (defun ,exp-maker-name (&rest operands)
         (make-instance ',exp-class-name :operands operands))

       (defmethod ,binary-name ((x ,exp-class-name) (y ,exp-class-name))
         (reduce #',binary-name (,merger (operands x) (operands y))))

       (defmethod ,binary-name ((x ,exp-class-name) y)
         (let ((last-xop (car (last (operands x)))))
           (if (abelian<= last-xop y)
               (let ((tmp (,binary-name last-xop y)))
                 (if (g/= tmp (,exp-maker-name last-xop y))
                     (apply #',exp-maker-name (append (operands x) (list y)))
                     (reduce #',binary-name (,merger (butlast (operands x)) (list tmp)))))
               (,binary-name y x))))

       (defmethod ,binary-name (x (y ,exp-class-name))
         (reduce #',binary-name (,merger (list x) (operands y))))

       (defmethod ,binary-name (x (y expression))
         (,exp-maker-name x y)))))

(defmacro defm-abelian-inverse-genop (gen-name inverse-name numeric-fn mult-fn
                                      &key
                                        (symbolic-name gen-name)
                                        (binary-name (format-symbol *abelian-binary-format* symbolic-name))
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
