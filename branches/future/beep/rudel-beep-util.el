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
;; This file contains miscellaneous utility functions that are used in
;; the BEEP backend.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rx) ;; for `rx'

(require 'rudel-transport-util) ;; for `rudel-transport-make-filter-stack'


;;; Constants
;;

(defconst rudel-beep-frame-header-regex
  (rx (group (1+ upper)) ?\s ;; Type
      (group (1+ digit)) ?\s ;; Channel
      (group (1+ digit)) ?\s ;; Message number
      (group (or ?. ?*)) ?\s ;; Final frame
      (group (1+ digit)) ?\s ;; First octet
      (group (1+ digit))     ;; Size
      ?\r ?\n)
  "This regular expression describes the header of a BEEP
frame.")


;;; Class rudel-beep-frame
;;

(defclass rudel-beep-frame ()
  ((type            :initarg :type ;; TODO use symbols here?
		    :type    string
		    :documentation
		    "A three-letter code specifying the type of
the message this frame is part of. Has to be one of MSG, RPY,
ERR, ANS and NUL.")
   (channel         :initarg :channel
		    :type    (integer 0)
		    :documentation
		    "The channel on the message of which the
frame is a part was sent.")
   (message-number  :initarg :message-number
		    :type    (integer 0)
		    :documentation
		    "The number of the message this frame is a
part of.")
   (final           :initarg :final
		    :type    boolean
		    :documentation
		    "Is this the final frame of its message?")
   (sequence-number :initarg :sequence-number
		    :type    (integer 0)
		    :documentation
		    "The number of the first octet of the frame
in the octet stream of the channel.")
   (payload         :initarg :payload
		    :type    string
		    :documentation
		    "The payload of this frame. The empty string
is a valid payload."))
  "This class represents a Beep frame which is a part of BEEP
message.")

(defmethod slot-missing ((this rudel-beep-frame)
			 slot-name operation &optional new-value)
  "Simulate slot :size of THIS."
  (cond
   ;; Slot :size, read only
   ((and (or (eq slot-name :size)
	     (eq slot-name 'size))
	 (eq operation 'oref))
    (with-slots (payload) this
      (string-bytes payload)))

   ;; We do not know the slot and/or operation; call next method.
   (t (call-next-method))))

(defmethod object-print ((this rudel-beep-frame) &rest strings)
  "Add most important slots of THIS to textual representation."
  (when (next-method-p)
    (with-slots (type
		 channel message-number
		 final sequence-number
		 size) this
      (apply
       #'call-next-method
       this
       (concat " " type)
       (format " chan: %d" channel)
       (format " msg: %d" message-number)
       (format " seq: %d" sequence-number)
       (format " size: %d (%s)" size (if final "final" "intermediate"))
       strings)))
  )


;;; Assembling frames
;;

(defun rudel-beep-assemble-frames (data)
  "Segment DATA into individual, complete BEEP frames."
  (let ((frames)
	(rest   (car data))
	(stored (cdr data)))
    (catch 'done
      ;; Repeat until there are no more frames left in REST.
      (while t
	;; TODO incorrect, since the payload could contain this sequence
	(let ((index (string-match-p "END\r\n" rest)))
	  (if index
	      (progn
		(push (concat
		       (mapconcat #'identity (reverse (cdr data)) "")
		       (substring rest 0 (+ index 5)))
		      frames)
		(setq rest   (substring rest (+ index 5))
		      stored nil))
	    (throw 'done nil)))))
    ;; Return complete frames and leftover data.
    (list
     frames
     (cons rest stored)))
  )


;;; Parsing and generating frames
;;

(defun rudel-beep-parse-frame (data)
  "Parse DATA as BEEP frame and return a `rudel-beep-frame' object.
This function signals `rudel-malformed-message' if DATA does not
constitute a valid BEEP frame."
  (let ((payload-start)
	(payload))
    ;; Process header
    (save-match-data
      ;; Match frame header
      (unless (string-match rudel-beep-frame-header-regex data) ;; TODO string-match-p?
	(signal 'rudel-malformed-message (list "malformed frame header")))

      ;; Extract header components.
      (let ((type     (match-string 1 data))
	    (channel  (string-to-number (match-string 2 data)))
	    (message  (string-to-number (match-string 3 data)))
	    (more     (string= (match-string 4 data) "."))
	    (sequence (string-to-number (match-string 5 data)))
	    (size     (string-to-number (match-string 6 data)))
	    ;; We drop the last five characters. Since the string can
	    ;; be multibyte (TODO prevent that?), this is safer than
	    ;; trying to take SIZE characters from the beginning.
	    (payload  (substring data (match-end 0) -5)))

	;; Construct frame.
	(rudel-beep-frame
	 (format "%s %d" type message)
	 :type            type
	 :channel         channel
	 :message-number  message
	 :final           more
	 :sequence-number sequence
	 :payload         payload))))
  )

(defun rudel-beep-generate-frame (frame)
  "Return string representation of FRAME."
  (with-slots (type
	       channel
	       message-number
	       final
	       sequence-number
	       size
	       payload) frame
    (format
     "%s %d %d %s %d %d\r\n%sEND\r\n"
     type
     channel message-number
     (if final "." "*") sequence-number
     size payload))
  )


;;; Transport filter stack convenience functions
;;

(defun rudel-beep-make-transport-filter-stack (transport)
  "Construct a filter stack for the frame level of the BEEP
protocol on top of TRANSPORT."
  (rudel-transport-make-filter-stack
   transport
   '(;; Assemble complete frames
     (rudel-assembling-transport-filter
      :assembly-function rudel-beep-assemble-frames)

     ;; Parse frames, leaving payload alone
     (rudel-parsing-transport-filter
      :parse-function    rudel-beep-parse-frame
      :generate-function rudel-beep-generate-frame)))
  )

(defun rudel-beep-make-channel-filter-stack (transport)
  "Construct a filter stack for the message level of the BEEP
channel protocol on top of TRANSPORT."
  (rudel-transport-make-filter-stack
   transport
   '(;; Make the filter stack stoppable and resumable.
     (rudel-buffering-transport-filter)

     ;; Assemble/fragment messages from/into frames.
     (rudel-assembling-transport-filter
      :assembly-function rudel-beep-message-assemble
      :fragment-function rudel-beep-message-fragment)

     ;; Separate/combine entity headers from payload and
     ;; parse/generate them.
     (rudel-parsing-transport-filter
      :parse-function    rudel-beep-message-parse-entities
      :generate-function rudel-beep-message-generate-entities)

     ;; Parse/generate payloads
     (rudel-parsing-transport-filter
      :parse-function    rudel-beep-message-parse-payload
      :generate-function rudel-beep-message-generate-payload)))
  )

(provide 'rudel-beep-util)
;;; rudel-beep-util.el ends here
