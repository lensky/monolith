(in-package #:monolith)

(defparameter +dp-as+
  (mapcar (lambda (row)
            (make-array
             7
             :element-type 'double-float
             :initial-contents (loop
                                  for i from 0 to 6
                                  for j = (if (< i (length row)) (float (elt row i) 1.0d0) 0.0d0)
                                  collect j)))
          '(()
            (1/5)
            (3/40 9/40)
            (44/45 -56/15 32/9)
            (19372/6561 -25360/2187 64448/6561 -212/729)
            (9017/3186 -355/33 46732/5247 49/176 -5013/18656)
            (35/384 0 500/1113 125/192 -2187/6784 11/84))))

(defparameter +dp-cs+
  (map '(vector double-float) (lambda (n) (float n 1.0d0))
   '(0 1/5 3/10 4/5 8/9 1 1)))

(defparameter +dp-bhat+
  (map '(vector double-float) (lambda (n) (float n 1.0d0))
   '(35/384 0 500/1113 125/192 -2187/6784 11/84 0)))

(defparameter +dp-b+
  (map '(vector double-float) (lambda (n) (float n 1.0d0))
   '(5179/57600 0 7571/16695 393/640 -92097/339200 187/2100 1/40)))

(defun rk-ks (f ti yi as cs step)
  (let* ((nks (length as))
         (nys (rows yi))
         (ks (make-matrix nks nys :element-type 'double-float)))
    (loop for i from 0 to (1- nks)
       for a = (elt as i)
       for c = (aref cs i)
       for ki = (m* step
                    (funcall
                     f
                     (+ ti (* step c))
                     (m+ yi (m* (transpose ks) a))))
       do (if (zerop nys)
              (setf (aref ks i) ki)
              (loop for j from 0 to (1- nys)
                   do (setf (aref ks i j) (aref ki j)))))
    (transpose ks)))

(defun rk-step (yi bs ks)
  (m+ yi (m* ks bs)))

(defun adjust-rk-step (step err tol order)
  (if (= 0 err)
      (* 2.0d0 step)
      (* step (max 0.01d0 (min (* 0.9d0 (expt (/ tol err) (/ 1.0d0 (1+ order)))) 2)))))

(defun rk54-step (f ti yi step tol &key (ds (dimensions yi)))
  (let* ((ks (rk-ks f ti yi +dp-as+ +dp-cs+ step))
         (y-4 (rk-step yi +dp-b+ ks))
         (y-5 (rk-step yi +dp-bhat+ ks))
         (err (let ((errs (m- y-4 y-5)))
                (if (null ds)
                    (abs errs)
                    (reduce (lambda (a b) (max a (abs b))) errs :initial-value 0.0d0))))
         (nstep (adjust-rk-step step err tol 4.0d0)))
    (if (< err tol)
        (values y-5 (+ ti step) nstep)
        (rk54-step f ti yi nstep tol))))
