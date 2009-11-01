;;; rudel-beep.el --- BEEP transport backend for Rudel
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, beep, transport, backend
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
;; 0.1 - initial version


;;; Code:
;;

(require 'rudel-backend)
(require 'rudel-transport)
(require 'rudel-transport-util)
(require 'rudel-tcp)

(require 'rudel-util)

(require 'rudel-beep-util) ;; TODO why
(require 'rudel-beep-channel)


;;; Constants
;;

(defconst rudel-beep-transport-version '(0 1)
  "Version of the BEEP transport backend for Rudel.")


;;; Class rudel-beep-backend
;;

(defclass rudel-beep-backend (rudel-transport-backend)
  ((capabilities :initform (connect)))
  "")

(defmethod initialize-instance ((this rudel-beep-backend) slots)
  ""
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-beep-transport-version))

(defmethod rudel-make-connection ((this rudel-beep-backend) info
				  &optional callback)
  "Connect to a BEEP peer using the information in INFO.
INFO has to be a property list containing at least the
keys :host, :port and :profiles.

The value of the :profile property should be a list of string
each of which should be an uri identifying one the profiles
supported by the initiating peer."
  (let* ((host           (plist-get info :host))
	 (profiles       (plist-get info :profiles))
	 ;; Create the underlying transport.
	 ;; TODO handle errors
	 (tcp-transport  (rudel-make-connection
			  (cdr (rudel-backend-get 'transport 'tcp))
			  info))
	 ;; Create a suitable stack of transport filters on top of the
	 ;; underlying transport.
	 (stack          (rudel-beep-make-transport-filter-stack
			  tcp-transport))
	 ;; Create the actual BEEP transport.
	 (beep-transport (rudel-beep-transport
			  (format "to %s" host)
			  :transport stack
			  :profiles  profiles)))

    ;; Now start the transport and wait until the connection has been
    ;; established.
    (rudel-start beep-transport)

    (rudel-state-wait
     (rudel-get-channel beep-transport 0)
     '(idle) nil ;;'(we-finalize they-finalize)
     callback)

    ;; Return the transport.
    beep-transport))


;;; Class rudel-beep-transport
;;

;; TODO store a mapping between profiles and channel-protocol classes?
;; would be useful when receiving </start> requests
(defclass rudel-beep-transport (rudel-transport-filter)
  ((profiles     :initarg :profiles
		 :type list
		 :documentation
		 "")
   (channels     :initarg :channels
		 :type hash-table
		 :documentation
		 "")
   (channel-zero :initarg :channel-zero
		 :type rudel-beep-channel-zero
		 :documentation
		 "")) ;; TODO put the channels into a separate dispatcher class?
  "")

(defmethod initialize-instance ((this rudel-beep-transport) slots)
  "Initialize THIS and register states."
  ;; Initialize slots of THIS.
  (when (next-method-p)
    (call-next-method))

  ;; Create channel list and channel 0
  (with-slots (profiles channels channel-zero) this
    (setq channels (make-hash-table))

    (let ((channel-0 (rudel-beep-channel-zero
		      "0"
		      :transport this
		      :start     `(greeting ,profiles))))
      (rudel-add-channel this channel-0)
      (setq channel-zero channel-0)))

  ;; Install a handler that passes received data to the user-provided
  ;; handler.
  (with-slots (transport) this
    (lexical-let ((this1 this))
      (rudel-set-filter
       transport
       (lambda (data)
	 (rudel-dispatch-frame this1 data)))))
  )

(defmethod rudel-register-state ((this rudel-beep-transport)
				 symbol state)
  "Associate THIS to STATE before registering STATE."
  ;; Associate THIS connection to STATE.
  (oset state :transport this)

  ;; Register the modified STATE.
  (when (next-method-p)
    (call-next-method))
  )

(defmethod rudel-close ((this rudel-beep-transport))
  ""
  ) ;; TODO
;; something like this
;; (rudel-send
;;   (find-channel this 0)
;;     (list "MSG" '(("close"
;;		  ("number"   . "1")
;;		  ("code"     . "200")
;;		  ("xml:lang" . "en"))
;;		 "Bla")))))

(defmethod rudel-dispatch-frame ((this rudel-beep-transport) frame) ;; TODO name
  ""
  (with-slots (channels) this
    (with-slots ((frame-channel :channel)) frame
      (let ((channel (gethash frame-channel channels)))
	(rudel-handle channel frame))))) ;; TODO name

(defmethod rudel-get-channel ((this rudel-beep-transport) id)
  ""
  (with-slots (channels) this
    (gethash id channels)))

(defmethod rudel-add-channel ((this rudel-beep-transport) channel)
  ""
  (with-slots (channels) this
    (with-slots (id) channel
      (when (gethash id channels)
	(signal 'duplicate-channel-id id))

      (puthash id channel channels))))

(defmethod rudel-create-channel ((this rudel-beep-transport) id profiles
				 &optional name) ;; TODO allow (null id) for auto-allocation?
  "Create a new channel with id ID on THIS connection using one of PROFILES.
ID should be an integer. The set of integers that are permissible
as channel ids depends on role of THIS.
PROFILES should be a list of strings each of which being the uri
of a BEEP profile."
  (let ((profile))
    ;; Request the allocation of a new channel on channel 0.
    (with-slots (channel-zero) this
      (unwind-protect
	  (progn
	    (rudel-switch channel-zero 'requesting profiles)
	    (destructuring-bind (symbol . state)
		(rudel-state-wait channel-zero
				  '(request-success) '(request-failure))
	      (with-slots ((profile1 :profile)) state
		(setq profile profile1))))
      ;; TODO we could catch -entered-error-state and re-signal; worth the effort?
	(rudel-switch channel-zero 'idle)))

    ;; Create the channel object and it to the channel hash-table.
    (let ((channel (rudel-beep-channel
		    (if name
			(format "%d (%s)" id name)
		      (format "%d" id))
		    :id        id
		    :profile   profile
		    :transport this)))
      (rudel-add-channel this channel)))
  )

(defmethod rudel-remove-channel ((this rudel-beep-transport) channel)
  "TODO"
  )

(defmethod rudel-close-channel ((this rudel-beep-transport) channel)
  "TODO"
  ;; Request closing CHANNEL on channel 0.
  (with-slots (channel-zero) this
    (rudel-switch channel-zero 'closing channel)
    (rudel-state-wait channel-zero '(idle))) ;; TODO handle errors

  ;;
  (rudel-remove-channel this channel))


;;; Autoloading
;;

;;;###autoload
(rudel-backend-register 'transport 'beep 'rudel-beep-backend)

(provide 'rudel-beep)
;;; rudel-beep.el ends here
