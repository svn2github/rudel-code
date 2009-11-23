;;; rudel-beep-tcp.el --- Flow control for BEEP over TCP transport
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, beep, tcp, flow control
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
;; This file contains the classes `rudel-beep-sequence-frame' and
;; `rudel-beep-tcp-flow-control' which implement flow control for BEEP
;; channels when using a TCP transport.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel-transport-util) ;; for `rudel-transport-filter'

(require 'rudel-beep-util) ;; for `rudel-beep-frame'


;;; Class `rudel-beep-sequence-frame'
;;

(defclass rudel-beep-sequence-frame (rudel-beep-frame)
  ((type           :initform "SEQ")
   (message-number :initform 0)
   (final          :initform t)
   (window         :initarg  :window
		   :type     (integer 0)
		   :documentation
		   "Number of bytes that can be received on a
channel without blocking.")
   (payload        :initform ""))
  "A special frame used with a TCP transport for flow control.")

(defmethod rudel-generate ((this rudel-beep-sequence-frame))
  "Return string representation of THIS frame."
  (with-slots (type channel sequence-number window) this
    (format
     "%s %d %d %d\r\n"
     type channel sequence-number window))
  )

;; TODO call next methods adds all the stuff from the base class again
(defmethod object-print ((this rudel-beep-sequence-frame) &rest strings)
  "Add most important slots of THIS to textual representation."
  (when (next-method-p)
    (with-slots (channel sequence-number window) this
      (apply
       #'call-next-method
       this
       (format " chan: %d" channel)
       (format " seq: %d" sequence-number)
       (format " wind: %d" window)
       strings)))
  )


;;; Class rudel-beep-tcp-flow-control
;;

(defclass rudel-beep-tcp-flow-control (rudel-transport-filter)
  ((seqno-in  :initarg  :seqno-in
	      :type     (integer 0)
	      :initform 0
	      :documentation
	      "Current sequence number for incoming messages."))
  "Objects of this class implement flow control for a BEEP
channel when spliced into a transport filter stack.")

(defmethod initialize-instance ((this rudel-beep-tcp-flow-control) slots)
  "Initialize slots of THIS and install filter."
  ;; Initialize slots.
  (when (next-method-p)
    (call-next-method))

  ;; Install filter into transport.
  (with-slots (transport) this
    (lexical-let ((this1 this))
      (rudel-set-filter
       transport
       (lambda (frame)
	 ;; Multiple dispatch would be very nice here.
	 (if (rudel-beep-sequence-frame-child-p frame)
	     ;; Handle sequence frame separately.
	     (rudel-handle-sequence-frame this1 frame)
	   ;; Handle normal frames by sending sequence frames and
	   ;; calling the filter.
	   (rudel-handle-normal-frame this1 frame))))))
  )

(defmethod rudel-handle-normal-frame ((this rudel-beep-tcp-flow-control)
				      frame)
  "Handle FRAME by increasing octet count and maybe sending a seq. frame."
  (with-slots (transport filter seqno-in) this
    ;; Increase octet sequence number for incoming octets.
    (incf seqno-in (oref frame :size)) ;; TODO possibly slow

    ;; Construct a sequence frame and send it.
    (let* ((channel   (oref frame :channel))
	   (seq-frame (rudel-beep-sequence-frame
		       "bla" ;; TODO
		       :channel         channel
		       :sequence-number seqno-in
		       :window          4096)))
      (rudel-send transport seq-frame))

    ;; When there is a filter pass FRAME to it.
    (when filter
      (funcall filter frame)))
  )

(defmethod rudel-handle-sequence-frame ((this rudel-beep-tcp-flow-control)
					frame)
  ""
  ;; TODO not implemented
  )

(provide 'rudel-beep-tcp)
;;; rudel-beep-tcp.el ends here
