;;;; fncl.asd
;;;;
;;;; Copyright (c) 2016 Sourav Datta [soura.jagat@gmail.com]

(asdf:defsystem #:fncl
  :description "FNCL is a collection of functional utlities for Common Lisp that the author frequently uses"
  :author "Sourav Datta [soura.jagat@gmail.com]"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "fncl")))

