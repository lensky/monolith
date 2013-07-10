(in-package #:monolith)

;; Normal distribution
(defun normal-pdf (x &key
                       (mean 0)
                       (stddev 1))
  (* (/ 1 (* stddev (sqrt (* 2 pi))))
     (exp (* -0.5 (expt (/ (- x mean) stddev) 2)))))
