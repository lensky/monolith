(asdf:defsystem monolith
  :description "monolith: a mathematical library for common lisp."
  :version "0.3"
  :author "YL"
  :depends-on (:iterate :water :phractal)
  :serial t
  :components ((:file "packages")
               (:module "generic-math"
                        :components ((:file "expressions")
                                     (:module "simplification" :depends-on ("expressions")
                                              :components ((:file "simplification")
                                                           (:file "pattern-simplifier")))
                                     (:module "operators" :depends-on ("expressions" "simplification")
                                              :serial t
                                              :components ((:file "operators")
                                                           (:file "monoid-operators")
                                                           (:file "abelian-operators")
                                                           (:file "primitive-operators")
                                                           (:file "supplemental-operators")
                                                           (:file "simplification-patterns")
                                                           (:file "derivative")))))
               (:file "matrix" :depends-on ("generic-math"))
               (:module "numerics" :depends-on ("matrix")
                        :components ((:file "ode")
                                     (:file "rand")
                                     (:file "ziggurat")
                                     (:file "primes")))))
