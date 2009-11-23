;;; rudel-beep-message.el --- Message class for use in the BEEP protocol
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, beep, protocol, message
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
;; This file contains the class `rudel-beep-message' and associated
;; utility functions.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rx) ;; for `rx'

(require 'eieio)

(require 'rudel-transport-util) ;; for `rudel-transport-make-filter-stack'

(require 'rudel-beep-util) ;; for `rudel-beep-frame'


;;; Constants
;;

(defconst rudel-beep-message-entities-regex
  (rx string-start (group (1+ anything) ?\r ?\n)
      ?\r ?\n) ;; Ends in double newline
  "This regular expression describes the entity header block that
can be present at the beginning of the payload in BEEP
messages.")


;;; Class rudel-beep-message
;;

;; TODO make entities accessible as virtual slots?
(defclass rudel-beep-message ()
  ((type            :initarg  :type
		    :type     string
		    :documentation
		    "A three-letter code specifying the type of
this message. Has to be one of MSG, RPY, ERR, ANS and NUL.")
   (channel         :initarg  :channel
		    :type     (integer 0)
		    :documentation
		    "The channel on this message.")
   (message-number  :initarg  :message-number
		    :type     (integer 0)
		    :documentation
		    "The number of this message")
   (sequence-number :initarg  :sequence-number
		    :type     (integer 0)
		    :documentation
		    "The number of the first octet of this
message in the octet stream of the channel.")
   (entities        :initarg  :entities
		    :type     list
		    :initform nil
		    :documentation
		    "MIME entity headers of this message
represented as a property list. The list can be empty.")
   (payload         :initarg :payload
		    :documentation
		    "The payload of this message. The empty
string is a valid payload."))
  "Objects of this class represent BEEP messages.")

(defmethod slot-missing ((this rudel-beep-message)
			 slot-name operation &optional new-value)
  "Simulate slot :size for THIS."
  (cond
   ;; Slot :size, read only
   ((and (or (eq slot-name :size)
	     (eq slot-name 'size))
	 (eq operation 'oref))
    (with-slots (entities payload) this
      (apply
       #'+
       (length payload)
       (if entities 2 0)
       (or (mapcar
	    (lambda (element)
	      (if (keywordp element)
		  (+ (length (symbol-name element)) 1)
		(+ (length element) 2)))
	    entities)
	   (list 0)))))

   ;; We do not know the slot and/or operation; call next method.
   (t (call-next-method)))
  )

(defmethod object-print ((this rudel-beep-message) &rest strings)
  "Add most important slots of THIS to textual representation."
  (when (next-method-p)
    (with-slots (type channel message-number sequence-number size) this
      (apply
       #'call-next-method
       this
       (concat " " type)
       (format " chan: %d" channel)
       (format " msg: %d"  message-number)
       (format " seq: %d"  sequence-number)
       (format " size: %d" size)
       strings))))


;;; Utility functions
;;

(defun rudel-beep-message-from-frames (frames)
  "Construct a message from FRAMES."
  (let* ((first-frame     (first frames))
	 (type            (oref first-frame :type))
	 (channel         (oref first-frame :channel))
	 (message-number  (oref first-frame :message-number))
	 (final           (oref first-frame :final))
	 (sequence-number (with-slots (sequence-number size) first-frame
			    (+ sequence-number size)))
	 (payload         (oref first-frame :payload)))
    ;; Process the rest of FRAMES ensuring consistency and
    ;; accumulating the data.
    (dolist (frame (rest frames))
      (with-slots ((type1            :type)
		   (channel1         :channel)
		   (message-number1  :message-number)
		   (final1           :final)
		   (sequence-number1 :sequence-number)
		   (payload1         :payload)
		   (size1            :size))
	  frame
	;; Ensure consistency
	(unless (string= type type1)
	  (signal 'rudel-malformed-message
		  (list "Inconsistent message type across frames")))
	(unless (= channel channel1)
	  (signal 'rudel-malformed-message
		  (list "Inconsistent channel id across frames")))
	(unless (= message-number message-number1)
	  (signal 'rudel-malformed-message
		  (list "Inconsistent message id across frames")))
	(when final
	  (signal 'rudel-malformed-message
		  (list "Additional frames after final frame")))
	(unless (= sequence-number sequence-number1)
	  (signal 'rudel-malformed-message
		  (list "Invalid octet sequence number in frame")))

	;; Update data
	(setq final           final1
	      sequence-number (+ sequence-number size1)
	      payload         (concat payload payload1))))

    ;; Ensure the last processed frame was the final frame of the
    ;; message.
    (unless final
      (signal 'rudel-malformed-message
	      (list "Incomplete message; final frame is missing")))

    ;; Create the message object.
    (rudel-beep-message
     (format "%s %d" type message-number)
     :type            type
     :channel         channel
     :message-number  message-number
     :sequence-number sequence-number
     :payload         payload))
  )


;;; Assembly and fragmentation of BEEP messages
;;

(defun rudel-beep-message-assemble (frames)
  "Assemble zero or more messages from FRAMES."
  (let ((messages)
	(message-frames))
    (dolist (frame frames)
      ;; Store FRAME. If it is the final frame of a message, assemble
      ;; the message.
      (push frame message-frames)
      (when (oref frame :final)
	(push (rudel-beep-message-from-frames message-frames)
	      messages)
	(setq message-frames nil)))
    ;; Return complete messages and the list of frame belonging to the
    ;; current unfinished message.
    (list messages message-frames))
  )

(defun rudel-beep-message-fragment (message)
  "Break MESSAGE into one or more frames and return them."
  (with-slots (type
	       channel message-number
	       sequence-number payload) message
    (list
     (rudel-beep-frame
      (format "%s %d" type message-number)
      :type            type
      :channel         channel
      :message-number  message-number
      :final           t
      :sequence-number sequence-number
      :payload         payload)))
  )


;;; Parsing and generating entity headers
;;

(defun rudel-beep-message-entities-from-payload (data)
  "Parse DATA as key-value pairs in multiple lines."
  (save-match-data
    (if (string-match rudel-beep-message-entities-regex data)
	;; Search entity headers in first match. Split match string
	;; into lines and lines into keys and values.
	(let* ((payload-start (match-end 0))
	       (entity-lines  (split-string (match-string 1 data)
					    "\r\n" t))
	       (entities      (apply
			       #'append
			       (mapcar
				(lambda (line)
				  ;; TODO wrong for "key:value" without
				  ;; space after :
				  (destructuring-bind (key value)
				      (split-string line ":")
				    (list (intern (concat ":" key))
					  (substring value 1))))
				entity-lines))))
	  (list entities (substring data payload-start)))
      (list nil data)))
  )

(defun rudel-beep-message-payload-from-entities (entities payload)
  "Serialize the property list ENTITIES as multi-line string.
The returned strings consists of the serialization of ENTITIES
concatenated with PAYLOAD."
  (if entities
      ;; When there are entities, concatenate entities and payload.
      (concat
       (mapconcat
	(lambda (element)
	  (if (keywordp element)
	      (format "%s: " (substring (symbol-name element) 1))
	    (format "%s\r\n" element)))
	entities
	"")
       "\r\n"
       payload)
    ;; Without entities, return just PAYLOAD.
    payload)
  )

(defun rudel-beep-message-parse-entities (message)
  "In MESSAGE, break entity headers out of the payload.
MESSAGE is returned after the modification."
  (with-slots (entities payload) message
    (destructuring-bind (entities1 payload1)
	(rudel-beep-message-entities-from-payload payload)
      (setq entities entities1
	    payload  payload1)))
  message)

(defun rudel-beep-message-generate-entities (message)
  "In MESSAGE, add serialized entity headers to the payload.
MESSAGE is returned after the modification."
  (with-slots (entities payload) message
    (setq payload (rudel-beep-message-payload-from-entities
		   entities payload)))
  message)


;;; Parsing and generating payloads
;;

(defun rudel-beep-message-parse-payload (message)
  "In MESSAGE, replace payload with parsed object.
The parsing is done according to the specified content type."
  (with-slots (entities payload) message
    ;; TODO do this in a less static way
    (let ((type (plist-get entities :Content-Type)))
      (cond
       ;; If the content type is BEEP+XML parse the payload as XML.
       ((string= type "application/beep+xml")
	;; TODO string->xml should work for ' delimiters
	(setq payload (string->xml (replace-regexp-in-string
				    "'" "\"" payload)))))))
  message)

(defun rudel-beep-message-generate-payload (message)
  "In MESSAGE, replace payload object with serialization.
The serialization is done according to the specified content
type."
  (with-slots (entities payload) message
    ;; TODO do this in a less static way
    (let ((type (plist-get entities :Content-Type)))
      (cond
       ;; If the content type is BEEP+XML assume the payload is an XML
       ;; data structure and convert it to a string.
       ((string= type "application/beep+xml")
	(setq payload (xml->string payload))))))
  message)

(provide 'rudel-beep-message)
;;; rudel-beep-message.el ends here
