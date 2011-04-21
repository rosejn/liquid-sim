(in-package :liquid)

(defclass EventNodeMixin ()
  ())

(defun event-label (label)
  (concatenate 'string "event:" label))

(defmethod add-handler ((src EventNodeMixin) (label String) handler)
  (link-new-node src (event-label label) handler))

;;(defmethod add-handler ((src EventNodeMixin) (label String) (handler EventNodeMixin))
 ;; (link src (event-label label) handler))

(defmethod fire-event ((src EventNodeMixin) label &rest args)
  (dolist (h (handlers src label))
	   (funcall h args)))

(defmethod handlers ((src EventNodeMixin) label)
  (mapcar
   #'(lambda (h) (value h))
   (out-nodes src (event-label label))))

(defmethod clear-events ((src EventNodeMixin) label)
  (remove-out-edges src (event-label label)))
  
