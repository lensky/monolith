(in-package #:monolith)

;; Basic abelian simplification patterns

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *abelian-arguments-matcher* (format-symbol "~c~a" #\@ (gensym)))
  (defvar *no-duplicate-keyword* :no-duplicate))

(defmacro add-abelian-patterns ((&key (duplicate-asymmetric nil)) &body specs)
  (labels ((intersperse (elt lst)
             (cons elt
                   (iter (for x in lst)
                         (nconcing (list x elt)))))
           (symmetric-p (pat) (equalp (reverse pat) pat))
           (construct-simp-spec (expr-sym pat action options)
             (let ((full-pat (cons expr-sym (intersperse *abelian-arguments-matcher* pat))))
               (cons full-pat (cons action options)))))
    `(add-simplifier-patterns
       ,@(iter (for ((expr-sym . pat) action . options) in specs)
               (for duplicate-rev = (and duplicate-asymmetric
                                         (not (eq (car options) *no-duplicate-keyword*))
                                         (not (symmetric-p pat))))
               (for new-options = (if (eq (car options) *no-duplicate-keyword*)
                                      (cdr options)
                                      options))
               (for new-action = `(apply #',expr-sym ,action
                                         (apply #'append ,*abelian-arguments-matcher*)))
               (nconcing (cons (construct-simp-spec expr-sym pat new-action new-options)
                               (if duplicate-rev
                                   (list (construct-simp-spec
                                          expr-sym (reverse pat) new-action new-options)))))))))

(add-simplifier-patterns
  ((g/+ @as 0 @as)
   (apply #'g/+ (apply #'append @as)))
  ((g/* @as 0 @as) 0)
  ((g/* @as 1 @as)
   (apply #'g/* (apply #'append @as))))

;; Addition
(add-abelian-patterns (:duplicate-asymmetric t)
 ((g/+ ?x ?x) (g/* 2 ?x))
 ((g/+ ?x (g/* @cs ?x @cs))
  (g/* (g/+ 1 (apply #'g/* (apply #'append @cs))) ?x))
 ((g/+ (g/* @cs ?x @cs) (g/* @ds ?x @ds))
  (g/* (g/+ (apply #'g/* (apply #'append @cs))
            (apply #'g/* (apply #'append @ds)))
       ?x)
  :no-duplicate))

(add-abelian-patterns (:duplicate-asymmetric t)
  ((g/* ?x ?x)
   (g/expt ?x 2))
  ((g/* ?x (g/expt ?x ?y))
   (g/expt ?x (g/+ 1 ?y)))
  ((g/* (g/expt ?x ?y) (g/expt ?x ?z))
   (g/expt ?x (g/+ ?y ?z))
   :no-duplicate))

;; Patterns for exponentials

(add-simplifier-patterns
  ((g/expt ?x 1) ?x)
  ((g/expt ?x 0) 1)
  ((g/expt 1 ?x) 1)
  ((g/expt 0 ?x) 0)
  ((g/expt (g/expt ?x ?y) ?z)
   (g/expt ?x (g/* ?y ?z))))

;; Trig stuff

(add-abelian-patterns (:duplicate-asymmetric t)
  ((g/* (g/tan ?x) (g/cos ?x)) (g/sin ?x))
  ((g/* (g/cot ?x) (g/sin ?x)) (g/cos ?x))
  ((g/* (g/cos ?x) (g/sec ?x)) 1)
  ((g/* (g/sin ?x) (g/csc ?x)) 1)
  ((g/* (g/cos ?x) (g/csc ?x)) (g// (g/tan ?x)))
  ((g/* (g/sin ?x) (g/sec ?x)) (g/tan ?x))
  ((g/+ (g/expt (g/cos ?x) 2) (g/expt (g/sin ?x) 2)) 1))

;; Other identities

(add-simplifier-patterns
  ((g/log (g/exp ?x)) ?x))
