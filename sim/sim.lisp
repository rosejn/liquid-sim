(defpackage :liquid-sim
  (:use :cl :liquid :space-time)
  (:export :go))

(in-package :liquid-sim)

(defclass Client ()
  ((graph :initarg graph
	  :accessor graph
	  :initform (make-graph))
   (root  :initarg root
	  :accessor root)))

(defmethod initialize-instance :after ((client Client) &key)
  (let ((name (slot-value client 'name)))
    (setf (slot-value client 'graph)
	  (make-instance 'graph :name name))
    (create-default-layout client)))

;;; TODO: Do this with an add-tree macro.
(defmethod create-default-layout ((client Client))
  (let* ((self (add-node (slot-value client 'graph) "self"))
	 (app-root (link-new-node self "apps" "app-root"))
	 (netmon-root (link-new-node app-root "net-monitor" "netmon-root"))
	 (chat-root (link-new-node app-root "chat" "chat-root"))
	 (net-root (link-new-node self "net" "net-root")))
    (link-new-node net-root "address" "none")
    (link-new-node netmon-root "num-peers" 0)
    (setf (slot-value client 'root) self)))

(defmethod set-address ((client Client) address)
  (insert-value (slot-value client 'root) '(net address) address))
    
(defun go ()
  (let ((sim (make-simulation))
