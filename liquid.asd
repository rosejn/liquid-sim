(in-package :cl-user)

(defpackage #:liquid-asd
  (:use #:cl #:asdf))

(in-package #:liquid-asd)

(defvar *liquid-version* "0.1")
(export '*liquid-version*)

(asdf:defsystem #:liquid
  :version #.*liquid-version*
  :depends-on (#:cl-graph)

  :components ((:module "lib"
	       :serial t ; Makes the components load in order
  	       :components
	      ((:file "package")
	       (:file "event")
	       (:file "graph")
	       (:file "client")))))
