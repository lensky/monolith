(in-package #:monolith)

;; Abelian operators

(defm-abelian-genop g/+ 0)

(defmethod g/+-gbin ((x number) (y number))
  (+ x y))

(defm-abelian-genop g/* 1)

(defmethod g/*-gbin ((x number) (y number))
  (* x y))

;; Binary operators

(defmgenop g/expt (base exponent))

(defmethod g/expt ((base number) (exponent number))
  (expt base exponent))

(defmethod g/expt ((base expression) (exponent expression))
  (make-g/expt-expr base exponent))
(defmethod g/expt ((base expression) (exponent number))
  (make-g/expt-expr base exponent))
(defmethod g/expt ((base number) (exponent expression))
  (make-g/expt-expr base exponent))

;; Inverse abelian operators

(defm-abelian-inverse-genop g/- g/+ #'- #'g/*)

(defm-abelian-inverse-genop g// g/* #'/ #'g/expt)

;; Unary operators

(defmacro defm-prim-unary-genop (gen-name prim-name arg)
  `(progn
     (defmgenop ,gen-name (,arg))
     (defmethod ,gen-name ((,arg number))
       (,prim-name ,arg))
     (defmethod ,gen-name ((,arg expression))
       (,(format-symbol *exp-maker-format* gen-name) ,arg))))

(defm-prim-unary-genop g/sin sin angle)
(defm-prim-unary-genop g/cos cos angle)
(defm-prim-unary-genop g/tan tan angle)
(defm-prim-unary-genop g/asin asin number)
(defm-prim-unary-genop g/acos acos number)
(defm-prim-unary-genop g/atan atan number)
(defm-prim-unary-genop g/sinh sinh number)
(defm-prim-unary-genop g/cosh cosh number)
(defm-prim-unary-genop g/tanh tanh number)
(defm-prim-unary-genop g/asinh asinh number)
(defm-prim-unary-genop g/acosh acosh number)
(defm-prim-unary-genop g/atanh atanh number)
(defm-prim-unary-genop g/exp exp number)
(defm-prim-unary-genop g/log log number)
(defm-prim-unary-genop g/sqrt sqrt number)

;; Support for symbols as variables

(defmethod g/+-gbin (x (y symbol))
  (make-g/+-expr x y))

(defmethod g/*-gbin (x (y symbol))
  (make-g/*-expr x y))

(defmethod g/expt ((base symbol) (exponent symbol))
  (make-g/expt-expr base exponent))
(defmethod g/expt ((base symbol) (exponent number))
  (make-g/expt-expr base exponent))
(defmethod g/expt ((base symbol) (exponent expression))
  (make-g/expt-expr base exponent))
(defmethod g/expt ((base number) (exponent symbol))
  (make-g/expt-expr base exponent))
(defmethod g/expt ((base expression) (exponent symbol))
  (make-g/expt-expr base exponent))

(defmethod g/sin ((angle symbol)) (make-g/sin-expr angle))
(defmethod g/cos ((angle symbol)) (make-g/cos-expr angle))
(defmethod g/tan ((angle symbol)) (make-g/tan-expr angle))
(defmethod g/asin ((number symbol)) (make-g/asin-expr number))
(defmethod g/acos ((number symbol)) (make-g/acos-expr number))
(defmethod g/atan ((number symbol)) (make-g/atan-expr number))
(defmethod g/sinh ((number symbol)) (make-g/sinh-expr number))
(defmethod g/cosh ((number symbol)) (make-g/cosh-expr number))
(defmethod g/tanh ((number symbol)) (make-g/tanh-expr number))
(defmethod g/asinh ((number symbol)) (make-g/asinh-expr number))
(defmethod g/acosh ((number symbol)) (make-g/acosh-expr number))
(defmethod g/atanh ((number symbol)) (make-g/atanh-expr number))
(defmethod g/exp ((number symbol)) (make-g/exp-expr number))
(defmethod g/log ((number symbol)) (make-g/log-expr number))
(defmethod g/sqrt ((number symbol)) (make-g/sqrt-expr number))
