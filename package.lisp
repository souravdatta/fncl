;;;; package.lisp
;;;;
;;;; Copyright (c) 2016 Sourav Datta [soura.jagat@gmail.com]

(defpackage #:fncl
  (:use #:cl)
  (:export :for :range :fold-right :fold-left :filter
	   :curry :$ :defcurry))

