(mapc 'require
      '(asdf
        asdf-install
        cl-graph
        ))

(defvar *lisp-packages-directory*
  (merge-pathnames "/home/rosejn/.sbcl/" (user-homedir-pathname)))

(push (list (merge-pathnames "site/" *lisp-packages-directory*)
            (merge-pathnames "systems/" *lisp-packages-directory*)
            "Local installation")
      asdf-install:*locations*)

(push (merge-pathnames "systems/" *lisp-packages-directory*)
      asdf:*central-registry*)

(defmacro load-or-install (package)
  `(handler-case
       (progn
         (asdf:operate 'asdf:load-op ,package))
     (asdf:missing-component ()
       (asdf-install:install ,package))))

;;(load (merge-pathnames
;;       "Library/Emacs/site-lisp/slime/swank-loader"
;;       (user-homedir-pathname)))
;;
;;(dolist (module '("swank-arglists"
;;                  "swank-asdf"
;;                  "swank-c-p-c"
;;                  "swank-fancy-inspector"
;;                  "swank-fuzzy"
;;                  "swank-presentation-streams"
;;                  "swank-presentations"))
;;  (load (merge-pathnames
;;         (merge-pathnames "Library/Emacs/site-lisp/slime/contrib/"
;;                          module)
;;         (user-homedir-pathname))))



;;(sb-ext:save-lisp-and-die "sbcl.core-with-slime")

(load "graph.lisp")
(load "test/graph_test.lisp")
(in-package :liquid-tests)

(defparameter g (make-instance 'Graph :name "Experiment Graph"))
(defparameter nodes nil)
(defparameter edges nil)

(defun linking-list (src_index name dst_index)
  (list (nth src_index nodes) name (nth dst_index nodes)))

(setf nodes nil)
(dotimes (n 7)
  (setf nodes (append nodes (list (add-node g n)))))

(setf edges nil)
(dolist (edge '((0 "d" 4)
		(1 "a" 0)
		(1 "b" 6)
		(1 "c" 2)
		(2 "g" 3)
		(4 "h" 3)
		(4 "f" 2)
		(6 "e" 5)
		(6 "e" 4)))
  (let ((src (nth (first edge) nodes))
	(name (second edge))
	(dst (nth (third edge) nodes))) 
    (setf edges (append edges
			(list (link g src name dst))))))
