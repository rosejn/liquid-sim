(defpackage :liquid-interface
  (:use :common-lisp)
  (:export :readlist)
)

(in-package :liquid-interface)

;; Read from a stream (stdin by default) and return the
;; string as a list object which can then be processed by lisp.
(defun readlist (&rest args)
  (values (read-from-string
            (concatenate 'string "("
                                 (apply #'read-line args)
                                 ")"))))

;; Ask a question and return the entered result
(defun prompt (&rest args)
  (apply #’format *query-io* args)
  (read *query-io*))


;; Create a REPL style loop like this
;;(break-loop #’eval #’(lambda (x) (eq x :q)) ">> ")
(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
    (let ((in (apply #'prompt args)))
      (if (funcall quit in)
          (return)
          (format *query-io* "~A~%" (funcall fn in))))))

