;;;; imap4.asd

(asdf:defsystem #:imap4
  :description "Describe imap4 here"
  :author "Mateusz Berezecki"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :depends-on
  ("esrap"
   "tls-1.3"
   "base64"
   "alien-ring")
  :components ((:file "package")
	       (:file "client")
               (:file "imap4")))
