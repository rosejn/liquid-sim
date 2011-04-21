(in-package :cl-user)

(defpackage #:liquid-sim-asd
  (:use #:cl #:asdf))

(in-package #:liquid-sim-asd)

(asdf:defsystem #:liquid-sim
  :depends-on (#:liquid #:space-time)

  :serial t ; Makes the components load in order
  :components ((:module "sim"
  	       :components
	      ((:file "sim")))))
