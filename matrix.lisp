(in-package #:monolith)

(defclass matrix ()
  ((elements :accessor elements
             :initarg :elements)
   (element-type :accessor element-type
                 :initarg :element-type)))

(defmethod print-object ((m matrix) stream)
  (loop for i from 0 to (1- (rows m))
       do (format stream "~a~%" (matrix-row m i))))

(defclass numeric-matrix (matrix)
  ((elements :accessor elements
             :initarg :elements
             :type (simple-array number))))

(defun make-matrix (rows cols &key (element-type t) (initial-contents nil) (initial-element 0))
  (let ((array (if (null initial-contents)
                   (make-array (list rows cols) :element-type element-type :initial-element initial-element)
                   (make-array (list rows cols) :element-type element-type :initial-contents initial-contents))))
    (case element-type
      ('number (make-instance 'numeric-matrix :elements array :element-type element-type))
      (otherwise (make-instance 'matrix :elements array :element-type element-type)))))

(defun matrix-ij (matrix row col)
  (aref (elements matrix) row col))

(defun (setf matrix-ij) (val matrix row col)
  (setf (aref (elements matrix) row col) val))

(defun matrix-row (matrix row)
  (let ((cols (cols matrix)))
   (make-array cols
               :displaced-to (elements matrix)
               :displaced-index-offset (* cols row))))

(defmethod (setf matrix-row) ((entries array) matrix row)
  (loop for entry across entries
       for col from 0 to (1- (length entries))
       do (setf (matrix-ij matrix row col) entry))
  entries)

(defun matrix-col (matrix col)
  (let ((rows (rows matrix)))
    (make-array rows
                :initial-contents
                (loop for row from 0 to (1- rows) collect (matrix-ij matrix row col)))))

(defmethod (setf matrix-col) ((entries array) matrix col)
  (loop for entry across entries
       for row from 0 to (1- (length entries))
       do (setf (matrix-ij matrix row col) entry))
  entries)

(defgeneric dimensions (s)
  (:documentation "Returns the dimensions of structure s."))
(defmethod dimensions (s) '())
(defmethod dimensions ((s array)) (array-dimensions s))
(defmethod dimensions ((s list))
  (cons (length s) (dimensions (car s))))
(defmethod dimensions ((s matrix))
  (dimensions (elements s)))

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

(defgeneric transpose (s)
  (:documentation "Transposes structure s."))

(defmethod transpose (s) s)

(defmethod transpose ((s array))
  (let* ((rs (rows s))
         (cs (cols s))
         (result (make-matrix cs rs :element-type (element-type s))))
    (loop for i from 0 to (1- rs)
       do (loop for j from 0 to (1- cs)
             do (setf (row-major-aref result (rc-to-index j i rs))
                      (row-major-aref s (rc-to-index i j cs)))))
    result))

(defmethod transpose ((m matrix))
  (setf (elements m) (transpose (elements m)))
  m)

(defun apply-elementwise (fn m1 &rest args)
  (let* ((d1 (dimensions m1))
         (entries (reduce #'* d1))
         (result (make-array d1 :element-type (element-type m1))))
    (loop for i from 0 to (1- entries)
       for x1 = (row-major-aref m1 i)
       for xs = (if args (mapcar (lambda (m) (row-major-aref m i)) args))
       do (setf (row-major-aref result i) (apply fn x1 xs)))
    result))

(defmethod simplify-exp ((expr array))
  (apply-elementwise #'simplify-exp expr))

(defmethod g/+-gbin ((x matrix) (y matrix))
  (apply-elementwise #'g/+ x y))

(defmethod g/+-gbin ((x numeric-matrix) (y numeric-matrix))
  (apply-elementwise #'+ x y))

(defmethod g/*-gbin ((x number) (y array))
  (apply-elementwise (lambda (n) (g/* x n)) y))

(defmethod g/*-gbin ((m1 matrix) (m2 matrix))
  (let* ((rows1 (rows m1))
         (rows2 (rows m2))
         (cols1 (cols m1))
         (cols2 (cols m2)))
    (if (= cols1 rows2)
        (let ((result (make-matrix rows1 cols2
                                   :element-type (if (eq (element-type m1)
                                                         (element-type m2))
                                                     (element-type m1)
                                                     t))))
          (loop for i from 0 to (1- rows1)
             for row-of-1 = (matrix-row m1 i)
             do (loop for j from 0 to (1- cols2)
                   for col-of-2 = (matrix-col m2 j)
                   do (setf (matrix-ij result i j)
                            (apply #'g/+ (map 'list #'g/* row-of-1 col-of-2)))))
          result)
        nil)))
