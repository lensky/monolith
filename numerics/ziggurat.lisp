(in-package #:monolith)

(defconstant +norm-x-255+ 3.6541528853610088d0)
(defconstant +norm-area-255+ 0.00492867323399d0)
(defconstant +255-divisor+ #.(expt 2.0d0 32.0d0))

(defun gaussian-f (x)
  (exp (/ (expt x 2.0d0) -2.0d0)))

(defun gaussian-f-inverse (x)
  (sqrt (* -2.0d0 (log x))))

(defun make-ziggurat-xs-table (n xn area f finv)
  (let ((array (make-array (1+ n) :element-type 'double-float :initial-element 0.0d0)))
    (setf (aref array n) xn)
    (loop for i from (1- n) downto 1
       for n+1 = (aref array (1+ i))
       do (setf (aref array i)
                (abs (funcall finv (+ (/ area n+1)
                                      (funcall f n+1))))))
    array))

(defun make-ziggurat-divided-table (xs-table f area &key (divisor +255-divisor+))
  (let ((divtable
         (map '(simple-array double-float 1) (lambda (x) (/ x divisor)) xs-table))
        (n (1- (length xs-table))))
    (setf (aref divtable 0) (/ (/ area (funcall f (aref xs-table n))) divisor))
    divtable))

(defun make-ziggurat-height-table (xs-table f)
  (map '(simple-array double-float 1) (lambda (x) (funcall f x)) xs-table))

(defun make-ziggurat-comparison-table (xs-table f area &key (divisor +255-divisor+))
  (let* ((n (1- (length xs-table)))
         (ctable (make-array (1+ n) :element-type 'double-float :initial-element 0.0d0)))
    (loop for i from 1 to n
       do (setf (aref ctable i)
                (* (/ (aref xs-table (1- i)) (aref xs-table i)) divisor)))
    (let ((xn (aref xs-table n)))
      (setf (aref ctable 0) (* (/ (* xn (funcall f xn)) area) divisor)))
    ctable))

(defparameter *norm-table-255*
  (make-ziggurat-xs-table 255 +norm-x-255+ +norm-area-255+ #'gaussian-f #'gaussian-f-inverse))

(defparameter *norm-table-255-divided*
  (make-ziggurat-divided-table *norm-table-255* #'gaussian-f +norm-area-255+))

(defparameter *norm-table-255-heights*
  (make-ziggurat-height-table *norm-table-255* #'gaussian-f))

(defparameter *norm-table-255-comparison*
  (make-ziggurat-comparison-table *norm-table-255* #'gaussian-f +norm-area-255+))

(defun sample-normal (&key
                        (rng (lambda () (random #.(expt 2 32))))
                        (mean 0.)
                        (variance 1.)
                        (divisor +255-divisor+)
                        (n 255)
                        (xn 3.654152885361009d0)
                        (norm-table-divided *norm-table-255-divided*)
                        (norm-table-heights *norm-table-255-heights*)
                        (norm-table-comparison *norm-table-255-comparison*))
  "Takes a generator for 32-bit random #'s, returns a normally distributed random value."
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type (function () (unsigned-byte 32)) rng)
           (type (simple-array double-float)
                 norm-table-divided
                 norm-table-heights
                 norm-table-comparison)
           (type fixnum n)
           (type double-float divisor xn mean variance))
  (let* ((rn (funcall (the function rng)))
         (i (logand rn n))
         (ci (aref norm-table-comparison i))
         (negative (logand rn (1+ n)))
         (U0 (float (funcall (the function rng))))
         (x (* U0 (aref norm-table-divided i))))
    (+
     mean
     (*
      variance
      (funcall
       (if (zerop negative) #'identity #'-)
       (if (< U0 ci)
           x
           (labels ((normalized-U ()
                      (the (double-float 0.0d0 1.0d0)
                        (/ (funcall (the function rng)) divisor))))
             (if (zerop i)
                 (loop
                    for x = (- (/ (the double-float (log (normalized-U))) xn))
                    for y = (- (the double-float (log (normalized-U))))
                    when (> (* 2 y) (expt x 2))
                    return (+ xn x))
                 (let* ((U1 (normalized-U))
                        (yi (aref norm-table-heights i))
                        (yi-1 (aref norm-table-heights (1- i)))
                        (y (+ (* U1 (- yi-1 yi)) yi)))
                   (if (< y (the double-float (gaussian-f x)))
                       x
                       (sample-normal :rng rng
                                      :mean 0
                                      :variance 1
                                      :divisor divisor
                                      :n n
                                      :xn xn
                                      :norm-table-divided norm-table-divided
                                      :norm-table-heights norm-table-heights
                                      :norm-table-comparison norm-table-comparison)))))))))))
