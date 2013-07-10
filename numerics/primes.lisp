(in-package #:monolith)

(defun sieve-of-eratosthenes (n &optional (existing-sieve (make-array 0 :element-type 'bit) existing-sieve-p))
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum n)
           (type simple-bit-vector existing-sieve))
  (let* ((sqtn (ceiling (sqrt n)))
         (sieve-dimensions (1- (ceiling (/ n 2))))
         (entries (1- sieve-dimensions))
         (c-indicator 1)
         (p-indicator 0))
    (labels ((is-p (i) (zerop i))
             (n->index (n)
               (declare (type fixnum n))
               (/ (- n 3) 2))
             (index->n (i) (+ (* 2 i) 3))
             (update-sieve (sieve p &optional (start (n->index (expt p 2))))
               (declare (type simple-bit-vector sieve)
                        (type fixnum p start))
               (loop for i from start to entries by p do
                    (setf (bit sieve i) c-indicator)))
             (refill-sieve (existing-sieve)
               (declare (type simple-bit-vector existing-sieve))
               (let ((old-sieve-n (1+ (* 2 (length existing-sieve))))
                     (nsieve (adjust-array existing-sieve sieve-dimensions)))
                 (declare (type fixnum old-sieve-n)
                          (type simple-bit-vector nsieve))
                 (loop for i from 3 to sqtn by 2 until (>= i old-sieve-n) do
                      (when (is-p (bit nsieve (n->index i)))
                        (let ((nstart (+ old-sieve-n (- i (mod old-sieve-n i)))))
                          (update-sieve nsieve i (n->index (if (evenp nstart) (+ i nstart) nstart))))))
                 nsieve)))
      (let ((start (if existing-sieve-p (index->n (1- (length existing-sieve))) 3))
            (sieve
             (if existing-sieve-p
                 (refill-sieve existing-sieve)
                 (make-array sieve-dimensions :element-type 'bit :initial-element p-indicator))))
        (loop for i from start to sqtn by 2 do
             (when (is-p (bit sieve (n->index i)))
               (update-sieve sieve i)))
        sieve))))

(defun get-nth-prime (n &optional (block-size (floor 1e7)) (existing-sieve (make-array 0 :element-type 'bit)))
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type simple-bit-vector existing-sieve)
           (type fixnum n block-size))
  (let* ((sieve (if (< 0 (length existing-sieve))
                    (sieve-of-eratosthenes block-size existing-sieve)
                    (sieve-of-eratosthenes block-size)))
         (nprimes (1+ (count 0 sieve))))
    (declare (type simple-bit-vector sieve))
    (labels ((extract-nth-prime (&optional (nprimes-left (1- n)) (start 0))
               (declare (type fixnum nprimes-left start))
               (if (zerop nprimes-left)
                   (+ (* 2 (1- start)) 3)
                   (extract-nth-prime (1- nprimes-left) (1+ (position 0 sieve :start start))))))
      (if (>= nprimes n)
          (extract-nth-prime)
          (get-nth-prime n (* 10 block-size) sieve)))))
