(defpackage :liquid-tests
    (:use :common-lisp :lisp-unit :liquid)
    (:export :test))

(in-package :liquid-tests)

(defmacro graph-test (name &rest body)
  `(define-test ,name
     (let ((g (make-live-graph)))
       ,@body)))

(graph-test graph-count-test
	    (assert-equal 0 (size g))
	    (make-node g "foo")
	    (assert-equal 1 (size g))
	    (make-node g "bar")
	    (assert-equal 2 (size g)))

(graph-test graph-edge-test
	    (let* ((a (make-node g "a"))
		   (b (make-node g "b"))
		   (c (make-node g "c"))
		   (e1 (link a "e1" b))
		   (e2 (link a "e2" c))
		   (e3 (link b "e3" c)))
	      (assert-equal 0 (in-edge-count a))
	      (assert-equal 1 (in-edge-count b))
	      (assert-equal 2 (in-edge-count c))
	      (assert-equal "e3" (name (car (in-edges c "e3"))))
	      (assert-equal (name e1) (name (car (in-edges b "e1"))))
	      (assert-equal 2 (in-edge-count c))
	    ))

(graph-test graph-dot-test
	    (let* ((a (make-node g "a"))
		   (b (make-node g "b"))
		   (c (make-node g "c"))
		   (e1 (link a "e1" b))
		   (e2 (link a "e2" c))
		   (e3 (link b "e3" c)))
	      (cl-graph:graph->dot g "test.dot")))

(graph-test event-test
  (let* ((a (make-node g "a"))
		 (b (make-node g "b"))
		 (c (make-node g "c"))
		 (e1 (link a "e1" b))
		 (e2 (link a "e2" c))
		 (e3 (link b "e3" c))
		 (count 0))
	(labels ((count-up (args) (setf count (+ 2 count)))
		 (count-down (args) (setf count (- count 1))))
	  (add-handler a "count" #'count-up)
	  (add-handler a "count" #'count-down)
	  (dotimes (i 10) (fire-event a "count")))
	(assert-equal 10 count)))

(graph-test do-tree-test
  (let ((root (make-node g "root")))
    (do-tree (root
	       foo (foo-root)
	       bar (bar-root
		    baz (baz-root))))
    (assert-equal baz-root (list-query root '("bar" "baz")))))

(defun load-edges (g)
  (mapcar (lambda (val)
	    (cons val (make-node g val)))
	  '(:foo :bar :baz)))

(defun test (&optional (debug nil))
  (progn
    (use-debugger debug)
    (lisp-unit:run-all-tests :liquid-tests)))

(setf g (make-live-graph))
(setf a (make-node g "a"))
(setf b (make-node g "b"))
(setf c (make-node g "c"))
(setf e1 (link a "foo" b))
(setf e2 (link-new-node a "bar" c))

(add-tree (a app
	     (app-root
	      chat (chat-root)
	      net-monitor (net-monitor-root))
	     net (net-root)))

(add-tree a (zoo (zoo-root)))
(link-new-node a "zoo" "zoo-root")
