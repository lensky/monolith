(in-package #:monolith)

;; Defines supplemental elementary operators that do not exist in scheme as simple primitives

(defun g/sec (x) (g// (g/cos x)))
(defun g/csc (x) (g// (g/sin x)))
(defun g/cot (x) (g// (g/tan x)))

(defun g/abs (x) (g/sqrt (g/expt x 2)))
