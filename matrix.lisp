(in-package #:monolith)

(defgeneric element-type (sequential-thing))

(defmethod element-type ((array array))
  (array-element-type array))

(defclass matrix ()
  ((elements :accessor elements
             :initarg :elements)
   (element-type :accessor element-type
                 :initarg :element-type)))

(defmethod print-object ((matrix matrix) stream)
  (format stream "[~a]" (elements matrix)))

(defclass numeric-matrix (matrix)
  ((elements :accessor elements
             :initarg :elements
             :type (simple-array number))))

(defun make-array-matrix (rows cols &key (element-type 'number) (initial-contents nil))
  (let ((dimensions (if (= 1 cols)
                        (list rows)
                        (list rows cols))))
    (if initial-contents
        (make-array dimensions :element-type element-type :initial-contents initial-contents)
        (make-array dimensions :element-type element-type))))

(defun make-matrix (rows cols &key
                                (element-type t)
                                (initial-contents nil)
                                (construct-array t)
                                (initial-element 0.0d0))
  (let* ((col-p (= 1 cols))
         (row-p (= 1 rows))
         (class
          (let ((numeric (member element-type '(double-float number))))
            (cond
              (row-p (if numeric 'numeric-row-matrix 'row-matrix))
              (col-p (if numeric 'numeric-col-matrix 'col-matrix))
              (t (if numeric 'numeric-matrix 'matrix)))))
         (array
          (if construct-array
              (let ((dimensions (cond (row-p (list cols))
                                      (col-p (list rows))
                                      (t (list rows cols)))))
                (cond
                  ((null initial-contents)
                   (make-array dimensions
                               :element-type element-type
                               :initial-element initial-element))
                  (t
                   (let ((initial-contents (let ((init-ds (dimensions initial-contents)))
                                             (cond ((equalp init-ds dimensions) initial-contents)
                                                   ((and (not (null (cdr dimensions)))
                                                         (null (cdr init-ds)))
                                                    (list initial-contents))
                                                   ((and (null (cdr dimensions))
                                                         (not (null (cdr init-ds))))
                                                    (if (= 1 (cols initial-contents))
                                                        (cond
                                                          ((typep initial-contents 'sequence)
                                                           (map 'list (lambda (x) (elt x 0)) initial-contents))
                                                          ((typep initial-contents 'array)
                                                           (iter (for i from 0 to (1- (rows initial-contents)))
                                                                 (collect (aref initial-contents i 0)))))
                                                        (elt initial-contents 0)))))))
                     (if (typep initial-contents 'array)
                         initial-contents
                         (make-array dimensions
                                     :element-type element-type
                                     :initial-contents initial-contents))))))
              initial-contents)))
    (make-instance class :elements array :element-type element-type)))

(defun sequence->matrix (sequence &key (element-type t) (construct-array t))
  (let ((rows (rows sequence))
        (cols (cols sequence)))
    (make-matrix rows cols
                 :element-type element-type
                 :initial-contents sequence
                 :construct-array construct-array)))

(defgeneric col-matrix-p (matrix))
(defmethod col-matrix-p (matrix)
  (let ((ds (dimensions matrix)))
    (or (null (cdr ds))
        (= (cadr ds) 1))))

(defgeneric row-matrix-p (matrix))
(defmethod row-matrix-p (matrix)
  (and (not (col-matrix-p matrix))
       (= 1 (car (dimensions matrix)))))

(defgeneric matrix-ij (matrix row col))

(defmethod matrix-ij ((matrix array) row col)
  (if (null (cdr (dimensions matrix)))
      (aref matrix row)
      (aref matrix row col)))

(defmethod matrix-ij ((matrix matrix) row col)
  (aref (elements matrix) row col))

(defmethod (setf matrix-ij) (val (matrix array) row col)
  (if (null (cdr (dimensions matrix)))
      (setf (aref matrix row) val)
      (setf (aref matrix row col) val)))

(defmethod (setf matrix-ij) (val (matrix matrix) row col)
  (setf (aref (elements matrix) row col) val))

(defgeneric matrix-row (matrix row))

(defmethod matrix-row :around (matrix row)
  (cond
    ((col-matrix-p matrix) (matrix-ij matrix row 0))
    ((row-matrix-p matrix) matrix)
    (t (call-next-method))))

(defmethod matrix-row ((matrix array) row)
  (let ((cols (cols matrix)))
    (make-array cols
                :element-type (element-type matrix)
                :displaced-to matrix
                :displaced-index-offset (* cols row))))

(defmethod matrix-row ((matrix matrix) row)
  (sequence->matrix (matrix-row (elements matrix) row)
                    :construct-array nil
                    :element-type (element-type matrix)))

(defmethod (setf matrix-row) (entries matrix row)
  (if (col-matrix-p matrix)
      (setf (matrix-ij matrix row 0) entries)
      (iter (for entry in-sequence entries with-index col)
            (setf (matrix-ij matrix row col) entry)))
  entries)

(defgeneric matrix-col (matrix col))

(defmethod matrix-col :around (matrix col)
  (cond ((row-matrix-p matrix) (matrix-ij matrix 0 col))
        ((col-matrix-p matrix) matrix)
        (t (call-next-method))))

(defmethod matrix-col ((matrix array) col)
  (let ((rows (rows matrix)))
    (make-array rows
                :element-type (element-type matrix)
                :initial-contents
                (loop for row from 0 to (1- rows) collect (matrix-ij matrix row col)))))

(defmethod matrix-col ((matrix matrix) col)
  (sequence->matrix (matrix-col (elements matrix) col) :construct-array nil :element-type (element-type matrix)))

(defmethod (setf matrix-col) (entries (matrix matrix) col)
  (if (row-matrix-p matrix)
      (setf (matrix-ij matrix 0 col) entries)
      (iter (for entry in-sequence entries with-index row)
            (setf (matrix-ij matrix row col) entry)))
  entries)

(defclass row-matrix (matrix) ())
(defclass col-matrix (matrix) ())
(defclass numeric-row-matrix (row-matrix numeric-matrix) ())
(defclass numeric-col-matrix (row-matrix numeric-matrix) ())

(defmethod row-matrix-p ((matrix row-matrix)) t)
(defmethod row-matrix-p ((matrix col-matrix)) nil)

(defmethod col-matrix-p ((matrix row-matrix)) nil)
(defmethod col-matrix-p ((matrix col-matrix)) t)

(defmethod matrix-ij ((matrix row-matrix) row col)
  (aref (elements matrix) col))
(defmethod matrix-ij ((matrix col-matrix) row col)
  (aref (elements matrix) row))

(defmethod (setf matrix-ij) (val (matrix row-matrix) row col)
  (setf (aref (elements matrix) col) val))
(defmethod (setf matrix-ij) (val (matrix col-matrix) row col)
  (setf (aref (elements matrix) row) val))

(defmethod (setf matrix-row) (entry (matrix row-matrix) row)
  (setf (elements matrix) entry))

(defmethod (setf matrix-col) (entry (matrix col-matrix) col)
  (setf (elements matrix) entry))

(defgeneric dimensions (s)
  (:documentation "Returns the dimensions of structure s."))
(defmethod dimensions (s) '())
(defmethod dimensions ((s array)) (array-dimensions s))
(defmethod dimensions ((s list))
  (if (null s)
      nil
      (cons (length s) (dimensions (car s)))))
(defmethod dimensions ((s matrix))
  (dimensions (elements s)))
(defmethod dimensions ((s row-matrix))
  (cons 1 (dimensions (elements s))))
(defmethod dimensions ((s col-matrix))
  (list (car (dimensions (elements s))) 1))

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

(defmethod transpose ((a array))
  (let* ((rs (rows a))
         (cs (cols a))
         (result (make-array-matrix cs rs)))
    (loop for i from 0 to (1- rs)
       do (loop for j from 0 to (1- cs)
             do (setf (matrix-ij result j i)
                      (matrix-ij a i j))))
    result))

(defmethod transpose ((m matrix))
  (let* ((rs (rows m))
         (cs (cols m))
         (result (make-matrix cs rs :element-type (element-type m))))
    (loop for i from 0 to (1- rs)
       do (loop for j from 0 to (1- cs)
             do (setf (matrix-ij result j i)
                      (matrix-ij m i j))))
    result))

(defmethod transpose ((m row-matrix))
  (make-matrix (cols m) 1 :element-type (element-type m) :initial-contents (elements m) :construct-array nil))
(defmethod transpose ((m col-matrix))
  (make-matrix 1 (rows m) :element-type (element-type m) :initial-contents (elements m) :construct-array nil))

(defmethod transpose ((m numeric-row-matrix))
  (make-matrix (cols m) 1 :element-type (element-type m) :initial-contents (elements m) :construct-array nil))
(defmethod transpose ((m numeric-col-matrix))
  (make-matrix 1 (rows m) :element-type (element-type m) :initial-contents (elements m) :construct-array nil))

(defmacro-driver (FOR var AS-INDEX dimensions)
  (with-gensyms (cmp lds i ds)
    `(progn
       (with ,ds = ,dimensions)
       (with ,cmp = (mapcar #'1- ,ds))
       (with ,lds = (1- (length ,ds)))
       (initially (setq ,var
                        (nconc (iter (repeat ,lds) (collect 0))
                               (list -1))))
       (,(if generate 'generate 'for) ,var next
         (or (iter (for ,i downfrom ,lds)
                   (cond
                     ((< (nth ,i ,var) (nth ,i ,cmp))
                      (incf (nth ,i ,var)) (leave ,var))
                     ((zerop ,i) (leave nil))
                     (t (setf (nth ,i ,var) 0))))
             (terminate))))))

(defun apply-elementwise (result-maker result-accumulator selector fn seq &rest seqs)
  (let* ((d1 (dimensions seq))
         (acc (lambda (passed-val accumulated)
                (let ((val (car passed-val))
                      (is (cdr passed-val))
                      (accumulated (or accumulated (funcall result-maker seq d1))))
                  (apply result-accumulator accumulated val is)))))
    (iter (for is as-index d1)
          (for x1 = (apply selector seq is))
          (for xs = (if seqs (mapcar (lambda (s) (apply selector s is)) seqs)))
          (accumulate (cons (apply fn x1 xs) is) by acc))))

(defun copy-matrix-dimensions (matrix &optional dimensions)
  (declare (ignorable dimensions))
  (make-matrix (rows matrix) (cols matrix) :element-type (element-type matrix)))

(defun copy-array-dimensions (array &optional dimensions)
  (let ((dimensions (or dimensions (dimensions array))))
    (make-array dimensions :element-type (array-element-type array))))

(defun matrix-apply-elementwise (fn seq &rest seqs)
  (apply #'apply-elementwise
         #'copy-matrix-dimensions
         (lambda (matrix val i j)
           (setf (matrix-ij matrix i j) val)
           matrix)
         #'matrix-ij
         fn seq seqs))

(defun array-apply-elementwise (fn seq &rest seqs)
  (apply #'apply-elementwise
         #'copy-array-dimensions
         (lambda (array val &rest is)
           (setf (apply #'aref array is) val)
           array)
         #'aref
         fn seq seqs))

(defmethod simplify-exp ((expr matrix))
  (matrix-apply-elementwise #'simplify-exp expr))

(defmethod g/+-gbin ((x array) (y array))
  (array-apply-elementwise #'+ x y))

(defmethod g/+-gbin ((x matrix) (y matrix))
  (matrix-apply-elementwise #'g/+ x y))

(defmethod g/+-gbin ((x numeric-matrix) (y numeric-matrix))
  (matrix-apply-elementwise #'+ x y))

(defmethod g/--gbin ((x array) (y array))
  (array-apply-elementwise #'- x y))

(defmethod g/--gbin ((x matrix) (y matrix))
  (matrix-apply-elementwise #'g/- x y))

(defmethod g/--gbin ((x numeric-matrix) (y numeric-matrix))
  (matrix-apply-elementwise #'- x y))

(defmethod g/*-gbin ((x number) (y array))
  (array-apply-elementwise (lambda (n) (* x n)) y))

(defmethod g/*-gbin ((x number) (y matrix))
  (matrix-apply-elementwise (lambda (n) (g/* x n)) y))

(defmethod g/*-gbin ((x number) (y numeric-matrix))
  (matrix-apply-elementwise (lambda (n) (* x n)) y))

(defm-monoid-genop m* 1)

(defmethod m*-gbin (x y)
  (g/* x y))

(defun inner (m1 m2 &optional (multiplication-fn #'g/*) (addition-fn #'g/+))
  (let ((rows1 (rows m1))
        (rows2 (rows m2))
        (cols1 (cols m1))
        (cols2 (cols m2)))
    (if (= cols1 rows2)
        (let* ((dimensions (cond
                             ((= 1 cols2) (list rows1))
                             (t (list rows1 cols2))))
               (result (make-array dimensions
                                   :element-type (if (eq (element-type m1)
                                                         (element-type m2))
                                                     (element-type m1)
                                                     t))))
          (loop for i from 0 to (1- rows1)
             for row-of-1 = (matrix-row m1 i)
             do (loop for j from 0 to (1- cols2)
                   for col-of-2 = (matrix-col m2 j)
                   do (setf (matrix-ij result i j)
                            (reduce addition-fn (array-apply-elementwise multiplication-fn row-of-1 col-of-2)))))
          result)
        nil)))

(defmethod m*-gbin ((m1 matrix) (m2 matrix))
  (sequence->matrix
   (inner (elements m1) (elements m2))))

(defmethod m*-gbin ((m1 numeric-matrix) (m2 numeric-matrix))
    (sequence->matrix
     (mm (elements m1) (elements m2))
     :element-type 'number))

(defmethod m*-gbin ((m1 row-matrix) (m2 col-matrix))
  (reduce #'g/+ (map 'list #'g/* (elements m1) (elements m2))))

(defmethod m*-gbin ((m1 col-matrix) (m2 row-matrix))
  (setf (elements m1) (map 'vector #'g/* (elements m1) (elements m2)))
  m1)

(defmethod m*-gbin ((m1 array) (m2 array))
  (mm m1 m2))
