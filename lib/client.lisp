(in-package :liquid)


#|
(defmethod layout ((client Client))
  (let ((self (add-node graph "self")))
    (add-tree self (app
		     (app-root
		      chat (chat-root)
		      net-monitor (net-monitor-root))
		     net (net-root)))))
|#
