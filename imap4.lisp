;;;; imap4.lisp

(in-package #:imap4)

(require :esrap)

(defvar *literal-length*)

(defun not-doublequote (c)
  (not (eql #\" c)))

(defun atom-specialp (c)
  (when
      (or (not (graphic-char-p c))
	  (member c '(#\( #\) #\{ #\space #\\ #\" #\] #\% #\*) :test #'char=))
    t))

(defun not-atom-specialp (c)
  (not (atom-specialp c)))

(defun not-right-square-bracket-p (c)
  (not (char= c #\])))

(defun is-char8-p (c)
  (and (>= (char-code c) 1)
       (<= (char-code c) 255)))

(defun is-astring-char-p (c)
  (not-atom-specialp c))

(defrule astring (or (+ (is-astring-char-p character)) string)
  (:function text))

(defrule escaped-quote (and #\\ #\")
  (:constant #\"))

(defrule escaped-backslash (and #\\ #\\)
  (:constant #\\))

(defrule string-char
    (or escaped-quote
	escaped-backslash
	(not-doublequote character)))

(defrule char8 (is-char8-p character))
(defrule atom-char (not-atom-specialp character))

(defrule quoted-string (and #\" (* string-char) #\")
  (:destructure
   (q1 string q2)
   (declare (ignore q1 q2))
   (text string)))

(defrule number (* (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
  (:lambda (list)
    (parse-integer (text list) :radix 10)))

(defrule digit-nz (or "1" "2" "3" "4" "5" "6" "7" "8" "9"))
(defrule digit (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
(defrule nz-number (and digit-nz (* digit))
  (:function text)
  (:lambda (arg)
    (parse-integer arg)))

(defrule crlf (and #\return #+off #\newline))

(defrule literal-string-length (and #\{ number #\})
  (:destructure (lb num rb)
		(declare (ignore lb rb))
		num))

(defun parse-literal-string (text position end)
  (multiple-value-bind (length newpos1 ok?)
      (parse 'literal-string-length text :start position :end end :junk-allowed t)
    (when ok?
      (let ((newstart newpos1)
	    (newend (+ newpos1 length)))
	(multiple-value-bind (literal newpos2 ok?)
	    (parse (list 'string length) text :start newstart :end newend)
	  (declare (ignore newpos2))
	  (when ok?
	    (values literal newend ok?)))))))

(defrule literal-string #'parse-literal-string)
(defrule string (or quoted-string literal-string))
(defrule atom (+ atom-char)
  (:function text)
  (:function string-upcase)
  (:function intern))

(defrule authenticate
    (and "AUTHENTICATE" #\space auth-type (* (and crlf base64)))
  (:destructure
   (a sp authtyp rest)
   (declare (ignore sp a rest))
   (list 'authenticate authtyp)))

(defrule auth-type atom)

(defrule base64
    (and
     (* (and base64-char base64-char base64-char base64-char))
     (? base64-terminal))
  (:function text))

(defrule base64-char
    (or (alpha-char-p character)
	(digit-char-p character)
	"+" "/"))

(defrule base64-terminal
    (or (and base64-char base64-char "==")
	(and base64-char base64-char base64-char "=")))

(defrule body (and #\( (or body-type-1part body-type-mpart) #\)))
(defrule body-extension
    (or nstring
	number
	(and #\( body-extension (* (and #\space body-extension)) #\))))

(defrule body-ext-1part
    (and body-fld-md5
	 (? (and #\space body-fld-dsp
		 (? (and #\space body-fld-lang
			 (? (and #\space body-fld-loc
				 (* (and #\space body-extension))))))))))

(defrule body-ext-mpart
    (and body-fld-param
	 (? (and #\space body-fld-dsp
		 (? (and #\space body-fld-lang
			 (? (and #\space body-fld-loc
				 (* (and #\space body-extension))))))))))

(defrule body-fields
    (and body-fld-param #\space
	 body-fld-id #\space
	 body-fld-desc #\space
	 body-fld-enc #\space
	 body-fld-octets))

(defrule body-fld-desc nstring)
(defrule body-fld-dsp (or (and #\( string #\space body-fld-param #\)) null))
(defrule body-fld-enc
    (or
     (and #\" (or "7BIT" "8BIT" "BINARY" "BASE64" "QUOTED-PRINTABLE") #\")
     string))
(defrule body-fld-id nstring)
(defrule body-fld-lang (or nstring (and #\( string (* (and #\space string)) #\))))
(defrule body-fld-loc nstring)
(defrule body-fld-lines number)
(defrule body-fld-md5 nstring)
(defrule body-fld-octets number)

(defrule body-fld-param
    (and #\( string #\space string (* (and #\space string #\space string)) #\)))

(defrule body-type-1part
    (and
     (or body-type-base body-type-msg body-type-text)
     (? (and #\space body-ext-1part))))

(defrule body-type-basic (and media-basic #\space body-fields))
(defrule body-type-mpart (and (+ body) #\space media-subtype (? (and #\space body-ext-mpart))))
(defrule body-type-msg
    (and media-message #\space
	 body-fields #\space
	 envelope #\space
	 body #\space
	 body-fld-lines))

(defrule body-type-msg
    (and media-message #\space
	 body-fields #\space
	 envelope #\space
	 body #\space
	 body-fld-lines))

(defrule body-type-text
    (and media-text #\space
	 body-fields #\space
	 body-fld-lines))

;; TODO: continue from here
(defrule capability (or (and "AUTH=" auth-type) atom)
  (:lambda (arg)
    (cond
      ((and (listp arg) (= (length arg) 2))
       (list 'auth (cadr arg)))
      (t arg))))

(defrule capability-data
    (and "CAPABILITY" (* (and " " capability))
	 (* (and " IMAP4rev1"))
	 (* (and #\space capability)))
  (:destructure
   (cap caps1 imap4cap caps2)
   (declare (ignorable caps2 imap4cap cap))
   (list 'capabilities (mapcar #'cadr caps1))))

(defrule null "NIL")
(defrule nstring (or string null))

(defrule address
    (and #\( addr-name #\space addr-adl #\space addr-mailbox #\space addr-host #\)))

(defrule addr-adl nstring)
(defrule addr-host nstring)
(defrule addr-mailbox nstring)
(defrule addr-name nstring)

(defrule append
    (and "APPEND" #\space mailbox
	 (? (and #\space flag-list))
	 (? (and #\space date-time))
	 #\space mailbox))

(defrule command
    (and tag " "
	 (or command-any command-auth command-nonauth command-select)
	 crlf))

(defrule command-any
    (or "CAPABILITY" "LOGOUT" "NOOP" x-command))

(defrule command-auth
    (or append create delete examine list lsub rename
	select status subscribe unsubscribe))

(defrule command-nonauth
    (or login authenticate))

(defrule command-select
    (or "CHECK"
	"CLOSE"
	"EXPUNGE"
	copy
	fetch
	store
	uid
	search))

(defrule continue-req
    (and "+" " " (or resp-text base64) crlf)
  (:destructure
   (cont-sign sp text-or-base64 crlf)
   (declare (ignore cont-sign sp crlf))
   (list 'continue text-or-base64)))

(defrule copy (and "COPY" " " sequence-set " " mailbox))
(defrule create (and "CREATE" " " mailbox))
(defrule date (or date-text (and #\" date-text #\")))
(defrule date-day (and digit digit))
(defrule date-day-fixed (or (and " " digit) (and digit digit)))
(defrule date-month
    (or "Jan" "Feb" "Mar" "Apr" "May" "Jun"
	"Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defrule date-text (and date-day "-" date-month "-" date-year))
(defrule date-year (and digit digit digit digit))

(defrule date-time
    (and #\" date-day-fixed "-" date-month "-" date-year
	 #\" time " " zone #\"))

(defrule delete (and "DELETE" " " mailbox))

(defrule envelope
    (and "(" env-date " "
	 env-subject " "
	 env-from " "
	 env-sender " "
	 env-reply-to " "
	 env-to " "
	 env-cc " "
	 env-bcc " "
	 env-in-reply-to " "
	 env-message-id ")"))

(defrule env-bcc (or (and "(" (+ address) ")") null))
(defrule env-cc (or (and "(" (+ address) ")") null))
(defrule env-date nstring)
(defrule env-from (or (and "(" (+ address) ")") null))
(defrule env-in-reply-to nstring)
(defrule env-message-id nstring)
(defrule env-reply-to (or (and "(" (+ address) ")") null))
(defrule env-sender (or (and "(" (+ address) ")") null))
(defrule env-subject nstring)
(defrule env-to (or (and "(" (+ address) ")") null))
(defrule examine (and "EXAMINE" " " mailbox))
(defrule fetch
    (and "FETCH" " " sequence-set " "
	 (or "ALL" "FULL" "FAST" fetch-att (and "(" fetch-att (* (and " " fetch-att)) ")"))))

(defrule envelope-lit "ENVELOPE" (:constant 'envelope))
(defrule fetch-att
    (or envelope-lit
	flags-lit
	"INTERNALDATE"
	(and "RFC822" (? (or ".HEADER" ".SIZE" ".TEXT")))
	(and "BODY" (? "STRUCTURE"))
	"UID"
	(and "BODY" section (? (and "<" number "." nz-number ">")))
	(and "BODY.PEEK" section (? (and "<" number "." nz-number ">")))))

(defrule flag
    (or
     "\\Answered"
     "\\Flagged"
     "\\Deleted"
     "\\Seen"
     "\\Draft"
     flag-keyword
     flag-extension)
  (:lambda (arg)
    (format t "arg=~a,type=~a~%" arg (type-of arg))
    (etypecase arg
      (string (intern (string-upcase (subseq arg 1))))
      (symbol arg))))

(defrule flag-extension (and #\\ atom)
  (:destructure
   (p a)
   (declare (ignore p))
   a))

(defrule flag-fetch (or flag "\\Recent")
  (:lambda (flg)
   (etypecase flg
     (symbol flg)
     (string (intern (string-upcase (subseq flg 1)))))))

(defrule flag-keyword atom)

(defrule flag-list
    (and "(" (? (and flag (* (and " " flag)))) ")")
  (:destructure
   (lp lst rp)
   (declare (ignore lp rp))
   (when lst
     (cons (car lst)
	   (mapcar #'cadr (cadr lst))))))

(defrule flag-perm
    (or flag "\\*"))

(defrule greeting
    (and "*" " " (or resp-cond-auth resp-cond-bye) crlf)
  (:destructure
   (lit sp resp crlf)
   (declare (ignore lit sp crlf))
   resp))

(defrule header-fld-name astring)

(defrule header-list
    (and "(" header-fld-name (* (and " " header-fld-name)) ")"))

(defrule list
    (and "LIST" " " mailbox " " list-mailbox))

(defrule list-mailbox (or (+ list-char) string))

(defrule list-char (or atom-char list-wildcards resp-specials))
(defrule list-wildcards (or "%" "*"))

(defrule login (and "LOGIN" " " userid " " password))
(defrule lsub (and "LSUB" " " mailbox " " list-mailbox))

(defrule mailbox
    (or "INBOX" astring))

(defrule mailbox-data
    (or
     (and "FLAGS" " " flag-list)
     (and "LIST" " " mailbox-list)
     (and "LSUB" " " mailbox-list)
     (and "SEARCH" " " (* (and " " nz-number)))
     (and "STATUS" " " mailbox " " "(" (? status-att-list) ")")
     (and number " " "EXISTS")
     (and number " " "RECENT"))
  (:destructure
   (a sp b)
   (declare (ignore sp))
   (cond
     ((numberp a)
      (list (intern b) a))
     (t
      (list (intern a) b)))))

(defrule mailbox-list
    (and "(" (? mbx-list-flags) ")"
	 " " (or quoted-string null) " " mailbox))

(defrule mbx-list-flags
    (or
     (and (* (and mbx-list-oflag " "))
	  mbx-list-sflag
	  (* (and " " mbx-list-oflag)))
     (and mbx-list-oflag
	  (* (and " " mbx-list-oflag)))))

(defrule mbx-list-oflag
    (or "\\Noinferiors" flag-extension))

(defrule mbx-list-sflag
    (or "\\Noselect" "\\Marked" "\\Unmarked"))

(defrule media-basic
    (and
     (or
      (and #\" (or "APPLICATION" "AUDIO" "IMAGE" "MESSAGE" "VIDEO") #\")
      string)
     " " media-subtype))

(defrule media-message
    (and #\" "MESSAGE" #\" " " #\" "RFC822" #\"))

(defrule media-subtype string)

(defrule media-text (and #\" "TEXT" #\" " " media-subtype))

(defrule fetch-lit "FETCH" (:constant 'fetch))
(defrule expunge-lit "EXPUNGE" (:constant 'expunge))

(defrule message-data
    (and nz-number " " (or expunge-lit (and fetch-lit " " msg-att)))
  (:destructure (num sp lst)
		(declare (ignore sp))
		(case lst
		  (expunge
		   (list 'msg-data num lst))
		  (t
		   (list 'msg-data (cons
				    (car lst)
				    (cddr lst)))))))

(defrule msg-att
    (and
     "("
     (or msg-att-dynamic msg-att-static)
     (* (and " " (or msg-att-dynamic msg-att-static)))
     ")")
  (:destructure
   (lb attr attrs rb)
   (declare (ignore lb rb))
   (format t "attr=~a, attrs=~a~%" attr attrs)
   (if attrs
       (cons 'msg-attributes (cons attr (cdar attrs)))
       (list 'msg-attributes attr))))

(defrule flags-lit "FLAGS" (:constant 'flags))

(defrule msg-att-dynamic
    (and
     flags-lit " " "(" (? (and flag-fetch (* (and " " flag-fetch)))) ")")
  (:destructure
   (lit sp lb lst rb)
   (declare (ignore sp lb rb))
   (format t "literal=~a,list=~a~%" lit lst)
   (destructuring-bind (a b) lst
       (if b
	   lst
	   a))))

(defrule msg-att-envelope
    (and "ENVELOPE" " " envelope)
  (:destructure
   (lit sp env)
   (declare (ignore lit sp))
   (list 'envelope env)))

(defrule msg-att-internaldate
    (and "INTERNALDATE" " " date-time)
  (:destructure (lit sp date-time)
		(declare (ignore lit sp))
		(list 'internaldate date-time)))

(defrule msg-att-uid
    (and "UID" " " uniqueid)
  (:destructure (lit sp id)
		(declare (ignore lit sp))
		(list 'uid id)))

(defrule msg-att-static
    (or
     msg-att-envelope
     msg-att-internaldate
     (and "RFC822" (? (or ".HEADER" ".TEXT")) " " nstring)
     (and "RFC822.SIZE" " " number)
     (and "BODY" (? "STRUCTURE") " " body)
     (and "BODY" section (? (and "<" number ">")) " " nstring)
     msg-att-uid))

(defrule password astring)

(defrule rename (and "RENAME" " " mailbox " " mailbox))

(defrule response
    (and (* (or continue-req response-data)) response-done)
  (:destructure
   (cont-or-data resp-done)
   (if cont-or-data
       (list (car cont-or-data) (list 'done resp-done))
       (list 'done resp-done))))

(defrule response-data
    (and "* " (or resp-cond-state
		  resp-cond-bye
		  mailbox-data
		  message-data
		  capability-data)
	 crlf)
  (:destructure
   (lit other crlf)
   (declare (ignore lit crlf))
   (list 'data other)))

(defrule response-done
    (or response-tagged response-fatal))

(defrule response-fatal
    (and "* " resp-cond-bye crlf)
  (:destructure
   (wildcard resp crlf)
   (declare (ignore wildcard crlf))
   resp))

(defrule response-tagged
    (and tag " " resp-cond-state crlf)
  (:destructure
   (tag space resp crlf)
   (declare (ignore space crlf))
   (list 'response
	 (cons 'tag
	       (cons tag resp)))))

(defrule resp-cond-auth
    (and (or "OK" "PREAUTH")
	 " " resp-text)
  (:destructure (type space text)
		(declare (ignore space))
		(list (intern type) text)))

(defrule resp-cond-bye
    (and "BYE " resp-text))

(defrule resp-cond-state
    (and (or "OK" "NO" "BAD") " " resp-text)
  (:destructure
   (status sp txt)
   (declare (ignore sp))
   (list 'status (intern status) 'text txt)))

(defrule resp-specials (or #\]))
(defrule resp-text
    (and (? (and "[" resp-text-code "]" " ")) text)
  (:destructure
   (resp-txt txt)
   (if resp-txt
       (destructuring-bind (lb txt2 rb sp) resp-txt
	 (declare (ignore lb rb sp))
	 (list txt2 (text txt)))
       (text txt))))

(defrule resp-parse "PARSE" (:constant 'parse))

(defrule resp-read-only "READ-ONLY" (:constant 'read-only))
(defrule resp-read-write "READ-WRITE" (:constant 'read-write))
(defrule resp-trycreate "TRYCREATE" (:constant 'trycreate))

(defrule resp-badcharset
    (and "BADCHARSET" (? (and "(" astring (* (and " " astring)) ")")))
  (:destructure
   (lit opt)
   (declare (ignore lit))
   (list 'badcharset opt)))

(defrule resp-uidnext
    (and "UIDNEXT " nz-number)
  (:destructure
   (lit num)
   (declare (ignore lit))
   (list 'uidnext num)))

(defrule resp-uidvalidity
    (and "UIDVALIDITY " nz-number)
  (:destructure
   (lit num)
   (declare (ignore lit))
   (list 'uidvalidity num)))

(defrule resp-unseen
    (and "UNSEEN " nz-number)
  (:destructure
   (lit num)
   (declare (ignore lit))
   (list 'unseen num)))

(defrule resp-perm-flags
    (and "PERMANENTFLAGS ("
	 (? (and flag-perm (* (and " " flag-perm)))) ")")
  (:destructure
   (lit lst)
   (declare (ignore lit))
   (list 'permanentflags lst)))

(defrule resp-alert "ALERT" (:constant 'alert))

(defrule resp-atom-text
    (and atom (+ (and " " (not-right-square-bracket-p character))) "]")
  (:destructure
   (atom txt)
   (cons atom (text txt))))

(defrule resp-text-code
    (or resp-alert
	resp-badcharset
	capability-data
	resp-parse
	resp-perm-flags
	resp-read-only
	resp-read-write
	resp-trycreate
	resp-uidnext
	resp-uidvalidity
	resp-unseen
	resp-atom-text)
  (:lambda (arg)
    (format t "type:~a~%" (type-of arg))
    (format t "arg:~a~%" arg)
   arg))

(defrule search
    (and "SEARCH"
	 (? (and " CHARSET " astring))
	 (+ (and " " search-key))))

(defrule search-key
    (or "ALL"
	"ANSWERED"
	(and "BCC " astring)
	(and "BEFORE " date)
	(and "BODY " astring)
	(and "CC " astring)
	"DELETED"
	"FLAGGED"
	(and "FROM " astring)
	(and "KEYWORD " flag-keyword)
	"NEW"
	"OLD"
	(and "ON " date)
	"RECENT"
	"SEEN"
	(and "SINCE " date)
	(and "SUBJECT " astring)
	(and "TEXT " astring)
	(and "TO " astring)
	"UNANSWERED"
	"UNDELETED"
	"UNFLAGGED"
	(and "UNKEYWORD " flag-keyword)
	"UNSEEN"
	"DRAFT"
	(and "HEADER " header-fld-name " " astring)
	(and "LARGER " number)
	(and "NOT " search-key)
	(and "OR " search-key " " search-key)
	(and "SENTBEFORE " date)
	(and "SENTON " date)
	(and "SENTSINCE " date)
	(and "SMALLER " number)
	(and "UID " sequence-set)
	"UNDRAFT"
	sequence-set
	(and "(" search-key (* (and " " search-key)) ")")))

(defrule section (and "[" (? section-spec) "]"))

(defrule section-msgtext
    (or "HEADER"
	(and "HEADER.FIELDS" (? ".NOT") header-list)
	"TEXT"))

(defrule section-part
    (and nz-number (* (and "." nz-number))))

(defrule section-spec
    (or section-msgtext
	(and section-part (? (and "." section-text)))))

(defrule section-text
    (or section-msgtext "MIME"))

(defrule select (and "SELECT " mailbox))

(defrule seq-number (or nz-number "*"))
(defrule seq-range (and seq-number ":" seq-number))

(defrule sequence-set (and (or seq-number seq-range) (* (and "," sequence-set))))
(defrule status (and "STATUS " mailbox " (" status-att (* (and " " status-att)) ")"))

(defrule status-att
    (or "MESSAGES"
	"RECENT"
	"UIDNEXT"
	"UIDVALIDITY"
	"UNSEEN")
  (:function intern))

(defrule status-att-list
    (and
     status-att " " number
     (* (and " " status-att " " number))))

(defrule tag astring)
(defrule text (+ text-char)
  (:function text))

(defun not-crlf-p (c)
  (not (member c '(#\return #\newline))))

(defrule text-char (+ (not-crlf-p character)))

(defrule time (and digit digit ":" digit digit ":" digit digit))

(defrule uid (and "UID " (or copy fetch search store)))
(defrule uniqueid nz-number)

(defrule userid astring)

(defrule x-command (and "X" atom))

(defrule zone (and (or "+" "-") digit digit digit digit))

(defun make-response-string (str)
  (format nil "~a~a~a" str #\return #\newline))

(defun make-response-cont-string ()
  (format nil "+ ~a~atag OK AUTH successful~a~a"
	  #\return #\newline
	  #\return #\newline))
