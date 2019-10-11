(in-package :imap4)

(declaim (optimize (debug 3) (speed 0)))
(defvar *input-stream*)

(defclass imap-context ()
  ((tag :initform 0 :accessor imap-tag)
   (pending-cmd :initform nil :accessor imap-pending-cmd)
   (data :initform nil :accessor imap-data)
   (buffer :initform nil :initarg :buffer :accessor imap-buffer)
   (handler :initform nil :initarg :handler :accessor imap-handler)))

(defun make-imap-context (handler)
  (make-instance 'imap-context
		 :handler handler
		 :buffer (alien-ring:make-binary-ring-stream 65536)))

(defun imap-cmd-pending-p (ctx)
  (not (null (imap-pending-cmd ctx))))

(defun make-sasl-plain (user password)
  (concatenate
   'string '(#\Nul) user '(#\Nul) password))

(defun read-imap-data (tls data)
  (let* ((ctx (tls::data tls))
	 (stream (imap-buffer ctx)))
    (alien-ring:stream-write-sequence stream data 0 (length data))
    (handle-imap-response tls)))

(defun handle-imap-response (tls)
  (let* ((ctx (tls::data tls))
	 (stream (imap-buffer ctx))
	 (size (alien-ring:stream-size stream))
	 (handler (imap-handler ctx)))
    (loop
       for line = (read-line (imap-buffer ctx) nil nil)
       then (read-line (imap-buffer ctx) nil nil)
       while line
       do
	 ;(format t "line=~a~%" line)
	 ;(format t "stream data size: ~a~%" (alien-ring:stream-size stream))
	 (cond
	   ((imap-cmd-pending-p ctx)
	    (setf (imap-data ctx) (concatenate 'string (imap-data ctx) line))
	    (let ((pending-tag (tag->str (imap-tag ctx))))
	      (when (string= (subseq line 0 (length pending-tag)) pending-tag)	    
		(when handler
		  (funcall handler tls (imap-data ctx))))))
	   (t
	    (when handler
	      (funcall handler tls line)))))))

(defun connect (host port user password)
  (let ((auth-data (base64-encode (make-sasl-plain user password))))
    (client-connect host port
		    #'imap-connected
		    #'read-imap-data)))

(defun add-crlf (str)
  (concatenate 'string str '(#\return #\newline)))

(defun make-tag (n)
  (format nil "cmd~a" n))

(defun tag->str (tag)
  (format nil "cmd~a" tag))

(defun make-cmd (tag cmd)
  (add-crlf
   (concatenate 'string (make-tag tag) '(#\space) cmd)))

(defun imap-connected (tls)
  (setf (tls::data tls) (make-imap-context #'read-imap-greeting)))

(defun read-imap-greeting (tls txt)
  (let ((ctx (tls::data tls))
	(greeting (parse 'greeting txt)))
    (format t "greeting=~a~%" greeting)
    ;; proceed to send CAPABILITY command
    (imap-cmd-capability tls)))

(defun imap-cmd-capability (tls)
  (let* ((ctx (tls::data tls))
	 (tag (incf (imap-tag ctx))))
    (tls-write tls (make-cmd tag "CAPABILITY"))
    (setf (imap-pending-cmd ctx) 'capability
	  (imap-handler ctx) #'read-capabilities)))

(defun read-capabilities (tls data)
  (let* ((ctx (tls::data tls))
	 (caps (parse 'response data)))
    (format t "capabilities=~a~%" caps)))
