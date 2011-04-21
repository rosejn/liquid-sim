(in-package :liquid)

(require 'kmrcl)

(defun read-gml-file (path)
  (parse-graph (kmrcl:read-file-to-strings))
  )

(defun parse-graph (lines)
  
  )
    
(defun find-start (lines)
  (cond ((null lines) nil)
	((equal "graph" (string-trim " " (car lines))) (cdr lines))
	('t (find-start (cdr lines)))))

