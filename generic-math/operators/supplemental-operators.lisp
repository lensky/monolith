(in-package #:monolith)

;; Defines supplemental elementary operators that do not exist in Common Lisp as simple primitives

(defmgenop g/sec (x))
(defmethod g/sec ((x number))
  (/ (cos x)))
(defmethod g/sec ((x expression))
  (make-g/sec-expr x))
(defmethod g/sec ((x symbol))
  (make-g/sec-expr x))

(defmgenop g/csc (x))
(defmethod g/csc ((x number))
  (/ (sin x)))
(defmethod g/csc ((x expression))
  (make-g/csc-expr x))
(defmethod g/csc ((x symbol))
  (make-g/csc-expr x))

(defmgenop g/cot (x))
(defmethod g/cot ((x number))
  (/ (tan x)))
(defmethod g/cot ((x expression))
  (make-g/cot-expr x))
(defmethod g/cot ((x symbol))
  (make-g/cot-expr x))
