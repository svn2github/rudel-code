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
;; This file contains the class `rudel-beep-channel' which a generic
;; BEEP protocol channel. A BEEP channel implements the
;; `rudel-transport-filter' interface and can thus be used as a
;; transport.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel-state-machine) ;; channel 0 is a state machine

(require 'rudel-transport) ;; channels are `rudel-transport-filter's

(require 'rudel-beep-state) ;; channel 0 is a state machine


;;; Class rudel-beep-channel
;;

(defclass rudel-beep-channel (rudel-transport-filter)
  ((id        :initarg  :id
	      :type     (integer 0)
	      :documentation
	      "Identifier of the channel.")
   (seqno-in  :initarg  :seqno-in
	      :type     (integer 0)
	      :initform 0
	      :documentation
	      "Current sequence number for incoming messages.")
   (seqno-out :initarg  :seqno-out
	      :type     (integer 0)
	      :initform 0
	      :documentation
	      "Current sequence number outgoing messages.")
   (octet-out :initarg  :octet-out
	      :type     (integer 0)
	      :initform 0
	      :documentation
	      "TODO")
   (profile   :initarg  :profile
	      :type     string
	      :documentation
	      "TODO"))
  "TODO")

(defmethod rudel-send ((this rudel-beep-channel) data
		       &optional type content-type seq-hack)
  "Send DATA through THIS channel."
  ;; Construct a frame and send it.
  (with-slots (transport id seqno-out octet-out) this
    (let ((frame (rudel-beep-frame
		  "bla" ;; TODO
		  :type      (or type "MSG") ;; TODO
		  :channel   id
		  :message   (or seq-hack seqno-out)
		  :sequence  octet-out
		  :entities  (unless content-type
			       '(:Content-Type "application/beep+xml")) ;; TODO
		  :payload   data)))
      (rudel-send transport frame)

      (incf seqno-out)
      (incf octet-out (oref frame :size))))
  )

(defmethod rudel-close ((this rudel-beep-channel))
  "Close THIS channel."
  (with-slots (transport) this
    (rudel-close-channel transport this))
  )

(defmethod rudel-handle ((this rudel-beep-channel) data) ;; TODO name accept is reserved for state machines ...
  ""
  (with-slots (filter) this
    (when filter
      (funcall filter data)))
  )

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


;;; Class rudel-beep-channel-zero-state-greeting
;;

(defclass rudel-beep-channel-zero-state-greeting (rudel-beep-state) ;; TODO have a separate base class for this?
  ()
  "Initial state of channel 0.")

(defmethod rudel-enter ((this rudel-beep-channel-zero-state-greeting)
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

(defmethod rudel-accept ((this rudel-beep-channel-zero-state-greeting) frame)
  "Accept peer's greeting message, then switch to \"established\" state."
  ;; TODO we expect something like
  ;; <greeting>
  ;; <profile uri='http://iana.org/beep/SASL/OTP' />
  ;; </greeting>
  'idle)


;;; Class rudel-beep-channel-zero-state-requesting
;;

(defclass rudel-beep-channel-zero-state-requesting (rudel-beep-state)
  ((profiles :initarg  :profiles
	     :initform nil
	     :type     list
	     :documentation
	     "List of profiles offered to remote peer."))
  "Channel 0 enters this state when requesting a new channel.")

(defmethod rudel-enter ((this rudel-beep-channel-zero-state-requesting)
			profiles1)
  "Request channel state then wait for the response."
  (with-slots (profiles) this
    ;; Save profiles for evaluating the peer's response.
    (setq profiles profiles1)

    ;; Send channel start request.
    (rudel-send
     this
     `(("start"
	("number" . "1"))
       ,@(mapcar
	  (lambda (profile)
	    `(("profile"
	       ("uri" . ,profile))))
	  profiles))))

  ;; Stay in this state until the peer's response arrives.
  nil)

(defmethod rudel-accept ((this rudel-beep-channel-zero-state-requesting)
			 frame)
  "Analyze peer's response and switch states accordingly."
  (with-slots (payload) frame
    (cond
     ;; A positive response looks like this:
     ;; <profile uri="http://iana.org/beep/SASL/OTP"/>
     ((string= (xml-tag-name payload) "profile")
      (with-slots (profiles) this
	(let ((profile (xml-tag-attr payload "uri")))
	  (if (member profile profiles)
	      (list 'request-success profile)
	    (list 'request-failure 'invalid-profile nil))))) ;; TODO make this an error condition?

     ;; Negative response.
     ((string= (xml-tag-name payload) "error")
      (with-tag-attrs ((code code number)
		       ;;(:text))           ;; TODO
		       )
		       payload
	(list 'request-failure 'remote-error (list :code code
						   :text 'text))))

     ;; Invalid response.
     (t
      (list 'request-failure 'invalid-response nil))))
  )


;;; Class rudel-beep-channel-zero-state-request-success
;;

(defclass rudel-beep-channel-zero-state-request-success (rudel-beep-state)
  ((profile :initarg :profile
	    :type    string
	    :documentation
	    "TODO"))
  "Channel 0 enters this state when requesting a channel
succeeds.")

(defmethod rudel-enter ((this rudel-beep-channel-zero-state-request-success)
			profile1)
  "TODO"
  ;; Save profile.
  (with-slots (profile) this
    (setq profile profile1))
  nil)


;;; Class rudel-beep-channel-zero-state-request-failure
;;

(defclass rudel-beep-channel-zero-state-request-failure (rudel-beep-state)
  ((condition :initarg :profile
	      :type    symbol
	      :documentation
	      "TODO")
   (data      :initarg :data
	      :type    list
	      :documentation
	      "TODO"))
  "Channel 0 enters this state when requesting a channel fails.")

(defmethod rudel-enter ((this rudel-beep-channel-zero-state-request-failure)
			condition1 data1)
  "TODO"
  ;; Save condition and error data.
  (with-slots (condition data) this
    (setq condition condition1
	  data      data1))
  nil)

(defclass rudel-beep-channel-zero-state-closing (rudel-beep-state)
  ()
  "Channel 0 enters this state when a channel is closed.")

(defmethod rudel-enter ((this rudel-beep-channel-zero-state-closing)
			channel)
  "Send the close message."
  (with-slots (id) channel
    (rudel-send
     this
     `(("close"
	("number" . ,(format "%d" id))
	("code"   . ,(format "%d" 200))))))

  nil)

(defmethod rudel-accept ((this rudel-beep-channel-zero-state-closing)
			 frame)
  "Check whether closing the state succeeded."
  (let* ((data (oref frame :payload)) ;; TODO
	 (name (xml-tag-name data)))
    (cond
     ;;
     ((string= name "ok")
      'idle)

     ;;
     (t
      'idle)))
  )


;;; Class rudel-beep-channel-zero-state-idle
;;

(defclass rudel-beep-channel-zero-state-idle (rudel-beep-state)
  ()
  "Established channels remain in this state until something happens.")

(defmethod rudel-accept ((this rudel-beep-channel-zero-state-idle) frame)
  "."
  (let* ((data (oref frame :payload)) ;; TODO
	 (name (xml-tag-name data)))
    (cond
     ;; Peer requests a channel.
     ((string= name "start")

      (rudel-send this (xml-tag-child (oref frame :payload) "profile") "RPY" nil 1)

      nil)

     ;; Peer requests closing a channel.
     ((string= name "close")
      nil)
     ;; when receiving a <close/>:
     ;; + goto closing state
     ;; + take care of unfinished stuff
     ;; finally:
     ;; (rudel-send this (list "RPY" '(("ok"))))
     ;; Note: closing channel "0" is special

     ;; Error
     ((string= name "error")
      nil)

     ;; Unexpected message
     (t
      nil)))
  )


;;; BEEP channel 0 state list
;;

(defvar rudel-beep-channel-zero-states
  '((greeting        . rudel-beep-channel-zero-state-greeting)
    (requesting      . rudel-beep-channel-zero-state-requesting)
    (request-success . rudel-beep-channel-zero-state-request-success)
    (request-failure . rudel-beep-channel-zero-state-request-failure)
    (closing         . rudel-beep-channel-zero-state-closing)
    (idle            . rudel-beep-channel-zero-state-idle))
  "BEEP channel states for channel 0.")


;;; Class rudel-beep-channel-zero
;;

(defclass rudel-beep-channel-zero (rudel-state-machine
				   rudel-beep-channel)
  ((id      :initform 0)
   (profile :initform "urn:invalid:invalid"))
  "Specialized class for channel 0.")

(defmethod initialize-instance ((this rudel-beep-channel-zero) slots)
  "Initialize THIS and register states."
  ;; Initialize slots of THIS.
  (when (next-method-p)
    (call-next-method))

  ;; Register states.
  (rudel-register-states this rudel-beep-channel-zero-states))

(defmethod rudel-register-state ((this rudel-beep-channel-zero)
				 symbol state)
  "Associate THIS to STATE before registering STATE."
  ;; Associate THIS connection to STATE.
  (oset state :transport this)

  ;; Register the modified STATE.
  (when (next-method-p)
    (call-next-method))
  )

(defmethod rudel-handle ((this rudel-beep-channel-zero) data)
  ""
  (rudel-accept this data))

(provide 'rudel-beep-channel)
;;; rudel-beep-channel.el ends here
