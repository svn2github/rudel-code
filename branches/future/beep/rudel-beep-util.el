;;; rudel-beep-util.el --- Miscellaneous functions for the Rudel BEEP backend
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, beep, miscellaneous
;; X-RCS: $Id:$
;;
;; This file is part of Rudel.
;;
;; Rudel is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Rudel is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Rudel. If not, see <http://www.gnu.org/licenses>.


;;; Commentary:
;;


;;; History:
;;
;; 0.1 - Initial revision


;;; Code:
;;

(require 'rudel-xml)

(require 'rudel-transport-util)


;;; Class rudel-beep-frame
;;

(defclass rudel-beep-frame ()
  ((type     :initarg :type
	     :type    string
	     :documentation
	     "")
   (channel  :initarg :channel
	     :type    (integer 0)
	     :documentation
	     "")
   (message  :initarg :message
	     :type    (integer 0)
	     :documentation
	     "")
   (sequence :initarg :sequence
	     :type    (integer 0)
	     :documentation
	     "")
   (entities :initarg :entities
	     :type    list
	     :documentation
	     "")
   (payload  :initarg :payload
	     :documentation
	     ""))
  "")

(defmethod slot-missing ((this rudel-beep-frame)
			 slot-name operation &rest new-value)
  ""
  ;; TODO check name + op
  (with-slots (entities payload) this
    (apply
     #'+
     4
     (length payload)
     (or (mapcar
	  (lambda (element)
	    (if (keywordp element)
		(+ (length (symbol-name element)) 1)
	      (+ (length element) 2)))
	  entities)
	 (list 0))))
  )

(defmethod object-print ((this rudel-beep-frame) &rest strings)
  ""
  (when (next-method-p)
    (with-slots (type channel message sequence size) this
      (apply
       #'call-next-method
       this
       (concat " " type)
       (format " chan: %d" channel)
       (format " msg: %d"  message)
       (format " seq: %d"  sequence)
       (format " size: %d" size)
       strings))))


;;; Assembling frames
;;

(defun rudel-beep-assemble-frame (data)
  ""
  (let ((frames)
	(rest   (car data))
	(stored (cdr data)))
    (catch 'done
      (while t
	(let ((index (string-match-p "END\r\n" rest)))
	  (if index
	      (progn
		(push (concat
		       (mapconcat #'identity (reverse (cdr data)) "")
		       (substring rest 0 index) )
		      frames)
		(setq rest   (substring rest (+ index 5))
		      stored nil))
	    (throw 'done nil)))))
    (list
     frames
     (cons rest stored)))
  )


;;; Parsing and generating frames
;;

(defun rudel-beep-parse-frame (frame)
  ""
  (save-match-data
    (let ((header)
	  (entities-start)
	  (entities)
	  (payload-start)
	  (payload))
      ;; Process header
      (string-match "\\([A-Z]+\\) \\([0-9]+\\) \\([0-9]+\\) \\(\\.\\|\\*\\) \\([0-9]+\\) \\([0-9]+\\)" frame)
      (let ((type     (match-string 1 frame))
	    (channel  (string-to-number (match-string 2 frame)))
	    (message  (string-to-number (match-string 3 frame)))
	    (more     (match-string 4 frame))
	    (sequence (string-to-number (match-string 5 frame)))
	    (size     (string-to-number (match-string 6 frame))))
	(setq header         (list :type     type
				   :channel  channel
				   :message  message
				   :more     more
				   :sequence sequence
				   :size     size)
	      entities-start (match-end 0))

	;; Process entities
	(string-match "\\(.*\\)\n\n" frame entities-start)
	(setq payload-start (match-end 0)
	      entities      (apply
			     #'append
			     (mapcar
			      (lambda (line)
				(destructuring-bind (key value)
				    (split-string line ":")
				  (list
				   (intern
				    (concat ":" (downcase key)))
				   value)))
			      (split-string (match-string 1 frame)
					    "\n" t))))

	;; Payload
	(setq payload (substring frame payload-start))

	;; Construct result
	(rudel-beep-frame
	 "bla"
	 :type     type
	 :channel  channel
	 :message  message
	 ;;:more
	 :sequence sequence
	 ;;:size
	 :entities entities
	 :payload  payload))))
  )

(defun rudel-beep-generate-frame (frame)
  ""
  (with-slots (type channel message sequence size entities payload) frame
    (format
     ;;Content-Type: application/beep+xml\n
     "%s %d %d . %d %d\n%s%s\nEND\n"
     type
     channel
     message
     sequence
     size
     (if entities
       (concat
	(mapconcat
	 (lambda (element)
	   (if (keywordp element)
	       (format "%s: " (substring (symbol-name element) 1))
	     (format "%s\n" element)))
	 entities
	 "")
	"\n")
       "")
     payload))
  )


;;; Parsing and generating payloads
;;

(defun rudel-beep-parse-payload (frame)
  ""
  (with-slots (entities payload) frame
    (let ((type (plist-get entities :content-type)))
      (cond
       ((string= type " application/beep+xml")
	(setq payload (string->xml (replace-regexp-in-string "'" "\"" payload)))))))
  frame
  )

(defun rudel-beep-generate-payload (frame)
  ""
  (with-slots (entities payload) frame
    (let ((type (plist-get entities :content-type)))
      (cond
       (t ;;(string= type " application/beep+xml")
	(setq payload (xml->string payload))))))
  frame)


;;; Transport filter stack convenience function
;;

(defun rudel-beep-make-transport-filter-stack (transport)
  "Construct an BEEP protocol filter stack on top of TRANSPORT."
  (rudel-transport-make-filter-stack
   transport
   '((rudel-assembling-transport-filter
      :assembly-function rudel-beep-assemble-frame)
     (rudel-parsing-transport-filter
      :parse-function    rudel-beep-parse-frame
      :generate-function rudel-beep-generate-frame)
     (rudel-parsing-transport-filter
      :parse-function    rudel-beep-parse-payload
      :generate-function rudel-beep-generate-payload))))

(provide 'rudel-beep-util)
;;; rudel-beep-util.el ends here
