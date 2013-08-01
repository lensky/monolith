(in-package #:monolith)

(defun format-symbol (format-string &rest args)
  (let ((string-args (mapcar (lambda (x) (if (symbolp x) (symbol-name x) x)) args)))
    (intern (string-upcase (apply #'format nil format-string string-args)))))

(defparameter *exp-class-format* "~a-expr")
(defparameter *exp-maker-format* "make-~a-expr")

(defvar *expression-class-table* (make-hash-table))
(defun expression-class (symbol)
  (gethash symbol *expression-class-table*))
(defun add-class-name (symbol exp-class-name)
  (setf (gethash symbol *expression-class-table*) exp-class-name))

(defmacro defmgenop (gen-name args &key
                                     (symbolic-name gen-name)
                                     (operator-name gen-name)
                                     (docstring '())
                                     (expression-superclass 'expression)
                                     (exp-class-name (format-symbol *exp-class-format* symbolic-name))
                                     (exp-maker t)
                                     (exp-maker-name (format-symbol *exp-maker-format* symbolic-name))
                                     (extra-exp-slots '()))
  (progn
    (add-class-name symbolic-name exp-class-name)
    `(progn
       (add-class-name ',symbolic-name ',exp-class-name)

       (defgeneric ,gen-name ,args ,@(if docstring (list :documentation docstring)))
       
       (defclass ,exp-class-name (,expression-superclass)
         ((operator :allocation :class
                    :reader operator
                    :initform #',operator-name
                    :type 'function)
          (operator-name :allocation :class
                         :reader operator-name
                         :initform ',symbolic-name
                         :type 'list)
          ,@extra-exp-slots))

       ,@(if exp-maker
             (list `(defun ,exp-maker-name ,args
                      (make-instance ',exp-class-name :operands (list ,@(extract-args args)))))))))
