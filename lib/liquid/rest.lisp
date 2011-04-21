(require 'drakma)
(require 'cxml)

(defpackage :rest
  (:use :cl
	:drakma
	:cxml)
  (:export :rest-get :rest-post :rest-delete :rest-put))

(in-package :rest)

;;(defvar *smugmug-rest-url* (puri:parse-uri "https://api.smugmug.com/hack/rest/1.2.0/"))

(defun rest-get (addr)
  (rest-request :rest-url addr))

(defun rest-post (addr value))

(defun rest-delete (addr))

(defun rest-put (addr value))

;; addr is either an http or https PURI:URI or normal URI string
(defun rest-request (&key session method parameters (rest-url *smugmug-rest-url*))
  (when session (push `("SessionID" . ,(session-id session)) parameters))
  (when method (push `("method" . ,method) parameters))
  (multiple-value-bind (stream
			status-code
			headers	      ; Dotted (key . value) pairs
			uri           ; Uri of the response, in case of a redirect
			http-stream
			must-close
			status-text)
      (drakma:http-request rest-url
                           :parameters parameters
                           :method :post
                           :want-stream t)
    (declare (ignore headers uri http-stream must-close))
    (unwind-protect
         (if (/= status-code 200)
             (error 'http-error
                    :code status-code
                    :message status-text)
             (let ((response (cxml:parse-stream stream (cxml-dom:make-dom-builder))))
               (if (string= "ok" (dom:get-attribute (dom:document-element response) "stat"))
                   (dom:first-child response)
                   (let ((err-element (elt (dom:get-elements-by-tag-name response "err") 0)))
                     (error 'rest-error
                            :code (parse-integer (dom:get-attribute err-element "code") :junk-allowed t)
                            :message (dom:get-attribute err-element "msg"))))))
      (close stream))))
