(asdf:defsystem monolith
  :description "monolith: a mathematical library for common lisp."
  :version "0.3"
  :author "YL"
  :depends-on ("water" "phractal")
  :serial t
  :components ((:file "packages")
               (:module "numerics"
                        :components ((:file "matrix")
                                     (:file "ode")
                                     (:file "rand")
                                     (:file "ziggurat")
                                     (:file "primes")))
               (:module "generic-math"
                        :serial t
                        :components ((:file "expressions")
                                     (:module "simplification"
                                              :serial t
                                              :components ((:file "simplification")
                                                           (:file "pattern-simplifier")))
                                     (:module "operators"
                                              :serial t
                                              :components ((:file "operators")
                                                           (:file "monoid-operators")
                                                           (:file "abelian-operators")
                                                           (:file "primitive-operators")
                                                           (:file "supplemental-operators")
                                                           (:file "derivative")))))))
