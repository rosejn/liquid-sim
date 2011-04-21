(in-package :cl-user)

(defpackage #:liquid-tests-asd
  (:use #:cl #:asdf))

(in-package #:liquid-tests-asd)

(asdf:defsystem #:liquid-tests
  :depends-on (#:liquid #:lisp-unit)

  :serial t ; Makes the components load in order
  :components ((:module "test"
  	       :components
	      ((:file "graph_test")))))
