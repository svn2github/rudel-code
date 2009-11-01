;;; rudel-beep-channel.el --- Channels used in the BEEP protocol
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, beep, channel
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

(require 'rudel-state-machine)

(require 'rudel-transport)

(require 'rudel-beep-state)



;;; Class rudel-beep-channel-state-greeting
;;

(defclass rudel-beep-channel-state-greeting (rudel-beep-state)
  ()
  "Initial state of channel 0.")

(defmethod rudel-enter ((this rudel-beep-channel-state-greeting)
			profiles)
  "Send greeting message."
  ;; Send greeting message with profiles.
  (rudel-send
   this
   `("greeting"
     ,@(mapcar
	(lambda (profile)
	  `(("profile"
	     ("uri" . ,profile))))
	profiles))
   "RPY")

  ;; Stay in this state until the peer's greeting message arrives.
  nil)

(defmethod rudel-accept ((this rudel-beep-channel-state-greeting) frame)
  "Accept peer's greeting message, then switch to \"established\" state."
  ;; TODO we expect something like
  ;; <greeting>
  ;; <profile uri='http://iana.org/beep/SASL/OTP' />
  ;; </greeting>
  'established)


;;; Class rudel-beep-channel-state-starting
;;

(defclass rudel-beep-channel-state-starting (rudel-beep-state)
  ()
  "Channel 0 enters this state when requesting a new channel.")

(defmethod rudel-enter ((this rudel-beep-channel-state-starting) profiles)
  "Request channel state then wait for the response."
  ;; Send channel start request.
  (rudel-send
   this
   `(("start"
      ("number" . "1"))
     ,@(mapcar
	(lambda (profile)
	  `(("profile"
	     ("uri" . ,profile))))
	profiles)))

  ;; Stay in this state until the peer's response arrives.
  nil)

(defmethod rudel-accept ((this rudel-beep-channel-state-starting) frame)
  "Analyze peer's response and switch back to \"established\" state."
  ;; TODO we expect something like
  ;; <profile uri='http://iana.org/beep/SASL/OTP' />
  'established)


;;; Class rudel-beep-channel-state-established
;;

(defclass rudel-beep-channel-state-established (rudel-beep-state)
  ()
  "Established channels remain in this state until something happens.")

(defmethod rudel-accept ((this rudel-beep-channel-state-established) frame)
  "Relay received message handler."
  nil) ;; TODO relay message

;; Maybe like this:
;; (with-slots (filter) this
;;   (when filter
;;     (with-slots (payload) frame
;;	(funcall filter payload)))))


;;; BEEP channel state list
;;

(defvar rudel-beep-channel-states
  '((greeting    . rudel-beep-channel-state-greeting)
    (starting    . rudel-beep-channel-state-starting)
    (established . rudel-beep-channel-state-established)
    )
  "BEEP channel states.") ;; TODO mainly valid for channel 0


;;; Class rudel-beep-channel
;;

(defclass rudel-beep-channel (rudel-state-machine
			      rudel-transport-filter)
  ((id        :initarg  :id
	      :type     (integer 0)
	      :documentation
	      "TODO")
   (seqno-in  :initarg  :seqno-in
	      :type     (integer 0)
	      :initform 0
	      :documentation
	      "TODO")
   (seqno-out :initarg  :seqno-out
	      :type     (integer 0)
	      :initform 0
	      :documentation
	      "TODO")
   (octet-out :initarg  :octet-out
	      :type     (integer 0)
	      :initform 0
	      :documentation
	      "TODO")
   (profile   :initarg :profile
	      :type string
	      :documentation
	      "TODO"))
  "TODO")

(defmethod initialize-instance ((this rudel-beep-channel) slots)
  "Initialize THIS and register states."
  ;; Initialize slots of THIS.
  (when (next-method-p)
    (call-next-method))

  ;; Register states.
  (rudel-register-states this rudel-beep-channel-states))

(defmethod rudel-register-state ((this rudel-beep-channel)
				 symbol state)
  "Associate THIS to STATE before registering STATE."
  ;; Associate THIS connection to STATE.
  (oset state :transport this)

  ;; Register the modified STATE.
  (when (next-method-p)
    (call-next-method))
  )

(defmethod rudel-send ((this rudel-beep-channel) data
		       &optional type content-type)
  "Send DATA through THIS channel."
  ;; Construct a frame and send it.
  (with-slots (transport id seqno-out octet-out) this
    (let ((frame (rudel-beep-frame
		  "bla" ;; TODO
		  :type      (or type "MSG") ;; TODO
		  :channel   id
		  :message   seqno-out
		  :sequence  octet-out
		  :entities  (unless content-type
			       '(:Content-Type "application/beep+xml")) ;; TODO
		  :payload   data)))
      (rudel-send transport frame)

      (incf seqno-out)
      (incf octet-out (oref frame :size))))
  )

(defmethod rudel-close ((this rudel-beep-channel))
  ""
  ;; TODO (rudel-switch this 'closing)
  )

;; when receiving a <close/>:
;; + goto closing state
;; + take care of unfinished stuff
;; finally:
;; (rudel-send this (list "RPY" '(("ok"))))
;; Note channel "0" is special

(defmethod object-print ((this rudel-beep-channel) &rest strings)
  "Add id and sequence info to the string representation of THIS."
  (when (next-method-p)
    (with-slots (id seqno-out octet-out) this
      (apply
       #'call-next-method
       this
       (format " id: %d" id)
       (format " seqout: %d" seqno-out)
       (format " octout: %d" octet-out)
       strings)))
  )


;;; Class rudel-beep-channel-zero
;;

(defclass rudel-beep-channel-zero (rudel-beep-channel)
  ((id :initform 0))
  "Specialized class for channel 0.")

(provide 'rudel-beep-channel)
;;; rudel-beep-channel.el ends here