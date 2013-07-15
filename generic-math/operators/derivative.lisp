(in-package #:monolith)

(defmgenop Dx (fn var))

(defmethod Dx (fn (var symbol))
  (if (eq fn var) 1 0))

(defmethod Dx ((fn g/+-expr) var)
  (apply #'g/+ (mapcar #'(lambda (fn) (Dx fn var)) (operands fn))))

(defmethod Dx ((fn g/--expr) var)
  (g/- (Dx (car (operands fn)) var)
       (Dx (apply #'g/+ (cdr (operands fn))) var)))

(defmethod Dx ((fn g/*-expr) var)
  (let ((first (car (operands fn)))
        (rest (cdr (operands fn))))
    (g/+ (apply #'g/* (Dx first var) rest)
         (g/* first (Dx (apply #'g/* rest) var)))))

(defmethod Dx ((fn g//-expr) var)
  (let ((hi (car (operands fn)))
        (low (apply #'g/* (cdr (operands fn)))))
    (let ((Dhi (Dx hi var))
          (Dlow (Dx low var)))
      (g// (g/- (g/* low Dhi) (g/* hi Dlow))
           (expt low 2)))))

(defmethod Dx ((fn g/expt-expr) var)
  (let ((b (car (operands fn)))
        (a (cadr (operands fn))))
    (let ((Db (Dx b var))
          (Da (Dx a var)))
      (g/+ (g/* fn Da (g/log b))
           (g/* a Db (g/expt b (g/- a 1)))))))

(defmacro def-unary-derivative ((class var) &body body)
  (with-gensyms (fn d-var tmp-res)
    `(defmethod Dx ((,fn ,class) ,d-var)
       (let ((,var (car (operands ,fn))))
         (let ((,tmp-res (progn ,@body)))
           (g/* ,tmp-res (Dx ,var ,d-var)))))))

(defmacro def-unary-derivatives (&body d-specs)
  `(progn
     ,@(loop for (in . out) in d-specs
            collect
            (let ((class (format-symbol "~a-expr" (car in)))
                  (var (cadr in))
                  (body out))
              `(def-unary-derivative (,class ,var) ,@body)))))

(def-unary-derivatives
  ((g/cos u) (g/- (g/sin u)))
  ((g/sin u) (g/cos u))
  ((g/tan u) (g/expt (g/sec u) 2))
  ((g/asin u) (g// (g/sqrt (g/- 1 (g/expt u 2)))))
  ((g/acos u) (g/- (g// (g/* (g/abs u) (g/sqrt (g/- (g/expt u 2) 1))))))
  ((g/sqrt u) (g/* 1/2 (g// (g/sqrt u))))
  ((g/exp u) (g/exp u))
  ((g/log u) (g// u)))
