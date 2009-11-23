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
;; This file contains the class `rudel-beep-channel' which represents
;; a generic BEEP protocol channel. A BEEP channel implements the
;; `rudel-transport-filter' interface and can thus be used as a
;; transport.
;;
;; The derived class `rudel-beep-channel-zero' implements the
;; specified behavior of the special channel 0.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(eval-when-compile
  (require 'cl)) ;; for remove*

(require 'rudel-util) ;; for `rudel-hook-object'

(require 'rudel-state-machine) ;; channel 0 is a state machine

(require 'rudel-transport) ;; channels are `rudel-transport-filter's

(require 'rudel-beep-state) ;; channel 0 is a state machine
(require 'rudel-beep-message)


;;; Class rudel-beep-channel
;;

(defclass rudel-beep-channel (rudel-transport-filter)
  ((id             :initarg  :id
		   :type     (integer 0)
		   :documentation
		   "Identifier of the channel.")
   (profile        :initarg  :profile
		   :type     string
		   :documentation
		   "The profile URI identifying the protocol
spoken on this channel.")
   (seqno-in       :initarg  :seqno-in
		   :type     (integer 0)
		   :initform 0
		   :documentation
		   "Current sequence number for incoming
messages.")
   (seqno-out      :initarg  :seqno-out
		   :type     (integer 0)
		   :initform 0
		   :documentation
		   "Current sequence number of outgoing
messages.")
   (octet-out      :initarg  :octet-out
		   :type     (integer 0)
		   :initform 0
		   :documentation
		   "Current outgoing octet number.")
   (injector       :initarg  :injector
		   :type     rudel-injecting-transport-filter
		   :documentation
		   "The injecting transport filter which is
somewhere in the transport filter stack. This is used to inject
received frames into the data stream of the channel.")
   (root-transport :initarg  :root-transport
		   :type     rudel-transport ;; TODO rudel-beep-transport?
		   :documentation
		   "The most underlying transport."))
  "Objects of this class represent BEEP channels.")

(defmethod initialize-instance ((this rudel-beep-channel) slots)
  "Initialize slots of THIS and setup transport connections."
  ;; Initialize slots of THIS.
  (when (next-method-p)
    (call-next-method))

  (with-slots (transport injector root-transport) this
    ;; Wrap the transport in an injector. We have to inject frames
    ;; since we receive them through function calls.
    (setq injector (rudel-injecting-transport-filter
		    "injector"
		    :transport transport))

    ;; Wrap the transport in a suitable filter stack.
    (setq root-transport transport)
    (setq transport      (rudel-beep-make-channel-filter-stack injector))

    ;; Install our handler as filter of the transport.
    (lexical-let ((this1 this))
      (rudel-set-filter
       transport (lambda (message)
		   (with-slots (filter) this1
		     (when filter
		       (funcall filter message)))))))
  )

(defmethod rudel-send ((this rudel-beep-channel) data
		       &optional type content-type number)
  "Send DATA through THIS channel."
  (with-slots (transport id seqno-out octet-out) this
    ;; Construct a message and send it. Use automatic message number
    ;; or NUMBER when present.
    (let* ((type    (or type "MSG")) ;; TODO avoid these bindings?
	   (number  (or number seqno-out))
	   (message (rudel-beep-message
		     (format "%s %d" type number)
		     :type            type
		     :channel         id
		     :message-number  number
		     :sequence-number octet-out
		     :entities        (unless content-type
					'(:Content-Type "application/beep+xml")) ;; TODO
		     :payload         data)))
      (rudel-send transport message)

      ;; Increase automatic sequence number in case one was
      ;; used. Update outgoing octet count.
      (unless number
	(incf seqno-out))
      (incf octet-out (oref message :size))))
  )

(defmethod rudel-close ((this rudel-beep-channel))
  "Close THIS channel."
  (with-slots (transport) this
    (rudel-close-channel transport this))
  )

(defmethod rudel-handle ((this rudel-beep-channel) frame) ;; TODO name accept is reserved for state machines ...
  "Inject FRAME into the transport filter stack of THIS."
  (with-slots (injector) this
    (rudel-inject injector frame))
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
  ((id       :initarg  :id
	     :type     (integer 0)
	     :documentation
	     "Id of the channel that is currently being
requested.")
   (profiles :initarg  :profiles
	     :type     list
	     :documentation
	     "List of profiles offered to remote peer."))
  "Channel 0 enters this state when requesting a new channel.")

(defmethod rudel-enter ((this rudel-beep-channel-zero-state-requesting)
			id1 profiles1)
  "Request channel state then wait for the response."
  (with-slots (id profiles) this
    ;; Save profiles for evaluating the peer's response.
    (setq id       id1
	  profiles profiles1)

    ;; Send channel start request.
    (rudel-send
     this
     `(("start"
	("number" . ,(format "%d" id)))
       ,@(mapcar
	  (lambda (profile)
	    `(("profile"
	       ("uri" . ,(if (stringp profile)
			     profile
			   (car profile))))
	      ,@(when (consp profile)
		  (list (cdr profile)))))
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
      (with-slots (id profiles) this
	(let ((profile (xml-tag-attr payload "uri")))
	  (if (member profile (mapcar (lambda (profile)
					(if (stringp profile)
					    profile
					  (car profile)))
				      profiles)) ;; TODO break this out?
	      (list 'request-success id profile)
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
  ((id      :initarg :id
	    :type    (integer 0)
	    :documentation
	    "The id of the new channel.")
   (profile :initarg :profile
	    :type    string
	    :documentation
	    "The profile the remote peer has chosen for the new
channel."))
  "Channel 0 enters this state when requesting a channel
succeeds.")

(defmethod rudel-enter ((this rudel-beep-channel-zero-state-request-success)
			id1 profile1)
  "Save ID1 and PROFILE1 in slots of THIS."
  ;; Save profile.
  (with-slots (id profile) this
    (setq id      id1
	  profile profile1))
  ;; Stay in this state; the transition back to the idle state has to
  ;; be triggered externally.
  nil)


;;; Class rudel-beep-channel-zero-state-request-failure
;;

(defclass rudel-beep-channel-zero-state-request-failure (rudel-beep-state)
  ((condition :initarg :profile
	      :type    symbol
	      :documentation
	      "A condition symbol describing the reason for the
failure.")
   (data      :initarg :data
	      :type    list
	      :documentation
	      "Data associated to the condition that is the
reason for the failure."))
  "Channel 0 enters this state when requesting a channel fails.")

(defmethod rudel-enter ((this rudel-beep-channel-zero-state-request-failure)
			condition1 data1)
  "Save CONDITION1 and DATA1 in slots of THIS."
  ;; Save condition and error data.
  (with-slots (condition data) this
    (setq condition condition1
	  data      data1))

  ;; Stay in this state; the transition back to the idle state has to
  ;; be triggered externally.
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

(defmethod rudel-accept ((this rudel-beep-channel-zero-state-idle) message)
  "Handle incoming frame FRAME."
  (with-slots (payload) message ;; TODO message types?
    (let ((name (xml-tag-name payload)))
      (cond
       ;; Peer requests a channel.
       ((string= name "start")
	;; TODO break this up into several functions?
	(let* (;; Extract id of the requested channel.
	       (id       (string-to-number (xml-tag-attr payload "number")))
	       ;; Extract supported profiles.
	       (profiles (mapcar
			  (lambda (tag)
			    (let ((uri      (xml-tag-attr tag "uri"))
				  (children (xml-tag-children tag)))
			      (cons uri
				    (when (xml-tag-text-p (car children))
				      (car children)))))
			  (remove*
			   "profile" (xml-tag-children payload)
			   :key #'xml-tag-name :test-not #'string=)))
	       ;; Call the channel request hook the decide what to do.
	       (selected (object-run-hook-with-args
			  this 'channel-requested-hook id profiles)))

	  ;; If the hook function selected a profile, use
	  ;; it. Otherwise deny the channel request.
	  (if selected

	      ;; If the channel requested hook selected a profile,
	      ;; acknowledge the request and create a channel object.
	      (progn
		;; Send the selected profile, thereby acknowledging
		;; the channel request.
		(rudel-send this
			    `(("profile" ("uri" . ,selected)))
			    "RPY" nil 1)

		;; Create the channel object and pass it to observers
		;; through the channel created hook.
		;; TODO should we create the channel here?
		(let ((channel (rudel-beep-channel
				(format
				 "%d req by listen" id)
				:id        id
				:profile   selected
				:transport (oref this :root-transport))))
		  (object-run-hook-with-args
		   this 'channel-created-hook channel)))

	    ;; If the channel requested hook did not select a profile,
	    ;; deny the channel request.
	    (rudel-send-error this 501 "channel request denied")))

	;; Stay in this state.
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
	nil))))
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
				   rudel-beep-channel
				   rudel-hook-object)
  ((id                     :initform 0)
   (profile                :initform "urn:invalid:invalid")
   ;; Hooks
   (channel-requested-hook :initarg  :channel-requested-hook
			   :type     list
			   :initform nil
			   :documentation
			   "This hook is run when a request to
create a channels is received.")
   (channel-created-hook   :initarg  :channel-created-hook
			   :type     list
			   :initform nil
			   :documentation
			   "This hook is run after a channel has
successfully been created."))
  "Specialized class for channel 0.")

(defmethod initialize-instance ((this rudel-beep-channel-zero) slots)
  "Initialize THIS and register states."
  ;; Initialize base classes with non-virtual initargs.
  (when (next-method-p)
    (call-next-method
     this (rudel-state-machine-strip-initargs slots)))

  ;; Register states.
  (rudel-register-states this rudel-beep-channel-zero-states)

  ;; Install our handler as filter of the transport.
  (with-slots (transport) this
    (lexical-let ((this1 this))
      (rudel-set-filter
       transport
       (lambda (frame)
	 (rudel-accept this1 frame)))))
  )

(defmethod rudel-register-state ((this rudel-beep-channel-zero)
				 symbol state)
  "Associate THIS to STATE before registering STATE."
  ;; Associate THIS connection to STATE.
  (oset state :transport this)

  ;; Register the modified STATE.
  (when (next-method-p)
    (call-next-method))
  )

(defmethod rudel-send-error ((this rudel-beep-channel-zero)
			     code text)
  "Send an error message with code CODE and description TEXT."
  (rudel-send this
	      `(("error" ("code" . ,(format "%d" code)))
		,@(when text (list text)))
	      "RPY"))

(provide 'rudel-beep-channel)
;;; rudel-beep-channel.el ends here
