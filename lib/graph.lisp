(in-package :liquid)

(defclass LiveGraph (cl-graph:graph-container)
  ()
  (:default-initargs
   :size 1000
   :name (gensym "Graph")
   :default-edge-type :directed
   :vertex-class 'Node))

(defclass Node (cl-graph:graph-container-vertex EventNodeMixin)())

(defclass Edge (cl-graph:graph-container-edge
		cl-graph:directed-edge-mixin
                                cl-graph:weighted-edge-mixin)())


(defun make-live-graph ()
  (make-instance 'LiveGraph))

(defmacro with-graph (graph-item body)
  "Provided a node or edge, run body in a context where the symbol 'graph' is defined as the current graph structure."
  `(let ((graph (graph ,graph-item)))
     (,@body)))

(defmethod make-node ((g LiveGraph) val)
  "Add a node with the given value to the Graph g."
  (if (eql 'Node (type-of val))
      (add-vertex g val)
      (add-vertex g (make-instance 'Node
				   :graph g
				   :element val))))

(defmethod make-edge ((src Node) name (tgt Node))
"Link the src Node with the tgt Node using an edge labeled name"
  (with-graph src
    (add-edge graph (make-instance 'Edge :graph graph
			       :vertex-1 src
			       :vertex-2 tgt
			       :value name))))

(defmethod link ((src Node) name (tgt Node))
  (make-edge src name tgt))

  
(defmethod link-new-node ((src Node) name value)
  (with-graph src
    (link src name (make-node graph value))))

;;; Node Functions
(defmethod print-object ((n Node) stream)
  (format stream "<N: ~A>" (value n)))

(defmethod nodep (obj)
  (eql 'Node (type-of obj)))

(defmethod value ((n Node))
  (element n))

(defmethod (setf value) ((n Node) value)
  (setf (element n) value))

(defmethod in-edges ((n Node) &optional name)
"The set of inward pointing edges.  Optionally filtered."

  (if name
      (source-edges n (name-test name))
      (source-edges n)))

(defmethod in-edge-count ((n Node))
  (source-edge-count n))

(defmethod in-nodes ((n Node) &optional name)
  (let ((ins (in-edges n name)))
    (mapcar #'vertex-1 ins)))

(defmethod out-edges ((n Node) &optional name)
  (if name
      (target-edges n (name-test name))
      (target-edges n)))

(defmethod remove-out-edges ((n Node) name)
  (dolist (edge (target-edges n (name-test name)))
    (with-graph edge
      (delete-edge graph edge))))

(defmethod out-nodes ((n Node) &optional name)
  (let ((outs (out-edges n name)))
    (mapcar #'vertex-2 outs)))

(defmethod out-edge-count ((n Node))
  (target-edge-count n))

(defmethod insert-value ((root Node) query value)
  (dolist (node (list-query root query))
    (setf (element node) value)))
  
(defmethod list-query ((root Node) query)
  (if (null query)
      root
      (let* ((query-term (if (symbolp (car query))
			     (symbol-name (car query))
			     (car query)))
	     (children (out-nodes root query-term)))
	  (metatilities:flatten
	   (mapcar
	    (lambda (child) (list-query child (cdr query)))
	    children)))))

(defmethod rquery ((root Node) &rest query)
  (print query)
  (if (null (car query))
      root
      (remove-if #'null
	(metatilities:flatten
	 (mapcar
	  (lambda (child) (apply #'rquery child (cdr query)))
	  (out-nodes root (car query)))))))
  
(defmethod subgraph-query ((root Node) query foo)
  )

(defmacro stick (var &body body)
  `(setf ,var (append ,var ,@body)))

(defun split-tree (tree)
  (let ((nodes nil)
	(edges nil))
    (push (car tree) nodes)
    (dolist (item (cdr tree))
      (if (listp item)
	  (multiple-value-bind (n e) (split-tree item)
	    (format t "adding ~A to ~A~&" (list (car item)) (car edges))
	    (nconc (car edges) (list (car item)))
	    (stick nodes n)
	    (stick edges  e))
	  (push (list (car nodes) item) edges)))
    (values nodes edges)))

;; A little DSL for creating tree structures in a graph
(defmacro add-tree (root . tree)
  (let ((links nil))
    (do ((iter_tree tree (cddr iter_tree)))
	 ((null iter_tree))
      (destructuring-bind (edge (target &optional . sub) &optional . more)
	  iter_tree
	  (format t "root: ~A~&edge: ~A~&target: ~A~&" root edge target)))))
	  (push `(link-new-node ,root (symbol-name ,edge) ,target) links)
	
      
      (let ((new-node (link-new-node root edge target)))
	(if (listp sub-tree) (do-tree (cons new-node sub-tree)))))))

	  (edge (car iter_tree) (car iter_tree))
	  (target (caadr iter_tree) (caadr iter_tree))
	  (sub-tree (cdadr iter_tree) (cdadr iter_tree)))

 #| 
  (let ((graph (gensym "graph")))
  `(let ((,graph (graph ,root)))
     (,@(multiple-value-bind (nodes edges) (split-tree tree)
	(append (mapcar #'(lambda (node)
			    `(add-node ,graph (symbol-name ',node)))
			nodes)
		(mapcar #'(lambda (edge)
			    `(add-edge ,(car edge)
				       ,(second edge)
				       ,(third edge)))
			edges)))))))
|#

(defmethod print-tree ((root Node) &optional (depth 1) (tabs 1))
  (format t "~A " root)
  (if (> depth 0)
      (progn
	(format t "~&")
	(dolist (edge (out-edges root))
	  (dotimes (i tabs) (format t "~T"))
	  (format t "~A " edge)
	  (print-tree (vertex-2 edge) (1- depth) (1+ tabs))))))

(defun do-tree (tree)
  "Create a tree rooted at the first node."
  (let ((root (car tree)))
   (do* ((iter_tree (cdr tree) (cddr iter_tree))
	 (edge (car iter_tree) (car iter_tree))
	 (target (caadr iter_tree) (caadr iter_tree))
	 (sub-tree (cdadr iter_tree) (cdadr iter_tree)))
	((null iter_tree))
     (format t "root: ~A~&edge: ~A~&target: ~A~&" root edge target)
     (let ((new-node (link-new-node root edge target)))
       (if (listp sub-tree) (do-tree (cons new-node sub-tree)))))))
     
  
(defmethod graph-map ())

;; Graham's special map function
;; fn is applied to the elements of a sequence
;; starting with start.  Each successive element
;; is returned by the successor function, and the mapping
;; finishes when the test function returns true.
(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

;; And Graham's recursive map for trees
(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar fn args))
             args)))



;;; Edge Functions
(defmethod print-object ((e Edge) stream)
  (format stream "-~A->" (name e)))

(defmethod name ((e Edge))
  (element e))

;;; Utility functions
(defun name-test (name)
  #'(lambda (e) (when (equal (string-downcase name)
			     (string-downcase (element e))) e)))

