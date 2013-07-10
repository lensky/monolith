;; Basic matrix library for Common Lisp 

;; Matricies are represented as standard CL arrays, a one-dimensional array
;; being treated as a single column. The m*+- operations allow scalars in any
;; argument, allowing "listable" programming, circa Mathematica or Matlab.

(in-package #:monolith)

(defgeneric dimensions (s)
  (:documentation "Returns the dimensions of structure s."))
(defmethod dimensions ((s array)) (array-dimensions s))
(defmethod dimensions ((s list))
  (cons (length s) (dimensions (car s))))
(defmethod dimensions (s) '())

(defun rows (s)
  (let ((ds (dimensions s)))
    (if (null ds)
        0
        (car ds))))

(defun cols (s)
  (let ((ds (dimensions s)))
    (cond
      ((null ds) 0)
      ((null (cdr ds)) 1)
      (t (cadr ds)))))

(defun rc-to-index (i j ncols)
  (+ j (* i ncols)))

(defun ds-from-rc (r c)
  (if (<= c 1) r (list r c)))

(defun make-matrix (rows cols &key (element-type t))
  "Creates a matrix/row vector of the proper dimensions."
  (let ((ds (ds-from-rc rows cols)))
    (values (make-array ds :element-type element-type)
            ds)))

;; Sufficiently general that this does not depend on any above functions
(defgeneric apply-elementwise (fun m1 &rest args)
  (:documentation "Apply a function element-wise to a pair of things."))

(defmethod apply-elementwise (fun (m1 number) &rest args)
  (apply fun m1 args))

(defmethod apply-elementwise (fun (m1 array) &rest args)
  (let* ((d1 (dimensions m1))
         (entries (reduce #'* d1))
         (result (make-array d1 :element-type (array-element-type m1))))
    (loop for i from 0 to (1- entries)
       for x1 = (row-major-aref m1 i)
       for xs = (mapcar (lambda (m) (row-major-aref m i)) args)
       do (setf (row-major-aref result i) (apply fun x1 xs)))
    result))

(defun m+ (m1 m2) (apply-elementwise #'+ m1 m2))
(defun m- (m1 m2) (apply-elementwise #'- m1 m2))

(defgeneric m* (m1 m2 &key)
  (:documentation "Multiply the two matricies/vectors/scalars. Must have compatible dimensions."))

(defmethod m* ((m1 number) (m2 array) &key)
  (apply-elementwise (lambda (n) (* n m1)) m2))

(defmethod m* ((m1 array) (m2 number) &key (element-type (array-element-type m1)))
  (m* m2 m1 :element-type element-type))

(defmethod m* ((m1 number) (m2 number) &key) (* m1 m2))

(defmethod m* ((m1 array) (m2 array) &key (element-type (array-element-type m1)))
  (let* ((rows1 (rows m1))
         (cols1 (cols m1))
         (rows2 (rows m2))
         (cols2 (cols m2)))
    (if (= cols1 rows2)
        (multiple-value-bind (result nds) (make-matrix rows1 cols2 :element-type element-type)
          (loop for i from 0 to (1- rows1)
             do (loop for j from 0 to (1- cols2)
                   do (setf (row-major-aref result (rc-to-index i j cols2))
                            (reduce #'+
                                    (loop for k from 0 to (1- cols1)
                                       collect (* (row-major-aref m1 (rc-to-index i k cols1))
                                                  (row-major-aref m2 (rc-to-index k j cols2))))))))
          (if (eq nds 1) (aref result 0) result))
        nil)))

(defgeneric transpose (s)
  (:documentation "Transposes structure s."))
(defmethod transpose (s) s)
(defmethod transpose ((s array))
  (let* ((rs (rows s))
         (cs (cols s))
         (result (make-matrix cs rs :element-type (array-element-type s))))
    (loop for i from 0 to (1- rs)
       do (loop for j from 0 to (1- cs)
             do (setf (row-major-aref result (rc-to-index j i rs))
                      (row-major-aref s (rc-to-index i j cs)))))
    result))
