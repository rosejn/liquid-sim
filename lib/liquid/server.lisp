(require :hunchentoot)

(defpackage :liquid-serve
  (:use :cl
	:hunchentoot)
  (:export :start-server))

(in-package :liquid-serve)

(setq *dispatch-table*
      `(,(create-prefix-dispatcher "/test" 'test-page)
	 ,(create-prefix-dispatcher "/about" 'about-page)))


(defun test-page ()
  (let ((name (parameter "name")))
    (if name
	(format nil "Hi, <b>~a</b>" name)
	"Name: <form action='/test' method='get'><input type='text' name='name' /><input type='submit' name='submit' /></form>")))

(defun about-page ()
  "<html><h1>Hunchentoot Demo</h1>This is a very simple demonstration of the Hunchentoot webserver.</html>")

(liquid-serve:start-server :port 8080)
