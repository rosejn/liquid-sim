(load "rest.lisp")

(defpackage :liquid-client
  (:use :cl
	:rest)
  (:export :connect))

(in-package :liquid-client)

(defun connect (addr)
  (rest:rest-get addr))
