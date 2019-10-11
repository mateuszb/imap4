;;;; package.lisp

(defpackage #:imap4
  (:use #:cl)
  (:import-from :esrap
		:add-rule
		:parse
		:rule
		:defrule
		:text
		:string
		:?
		:*
		:!
		:&)
  (:import-from :base64 :base64-decode :base64-encode)
  (:import-from :tls
		:client-connect
		:tls-write)
  (:import-from :alien-ring
		:make-binary-ring-stream))
