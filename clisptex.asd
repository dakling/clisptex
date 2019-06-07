;;;; clisptex.asd

(asdf:defsystem #:clisptex
  :description "Generate LaTeX code using common lisp"
  :author "Dario Klingenberg <dario dot klingenberg at web dot de>"
  :license  "GPL3"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "clisptex")))
