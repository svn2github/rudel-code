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

(require 'rudel-beep-util)


;;; Constants
;;

(defconst rudel-beep-transport-version '(0 1)
  "Version of the BEEP transport backend for Rudel.")


;;; Class rudel-beep-backend
;;

(defclass rudel-beep-backend (rudel-transport-backend)
  ()
  "")

(defmethod initialize-instance ((this rudel-beep-backend) &rest slots)
  ""
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-beep-transport-version))

(defmethod rudel-make-connection ((this rudel-beep-backend) info
				  &optional callback)
  "Connect to an BEEP server using the information in INFO.
INFO has to be a property list containing at least the
keys :host, :port and :jid."
  (let* ((host           (plist-get info :host))
	 (profiles       (plist-get info :profiles)) ;; TODO use these
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
			  host
			  :transport stack))) ;; TODO use profiles here

    ;; Now start the transport and wait until the connection has been
    ;; established.
    (rudel-start beep-transport)

    (rudel-state-wait
     (with-slots (channels) beep-transport
       (gethash 0 channels))
     '(established) ;;'(we-finalize they-finalize)
     ;;callback)
     )

    ;; Return the transport.
    beep-transport))


;;; Class rudel-beep-transport
;;

(defclass rudel-beep-transport (rudel-transport-filter)
  ((channels :initarg :channels
	     :type hash-table
	     :documentation
	     "")) ;; TODO put the channels into a separate dispatcher class?
  "")

(defmethod initialize-instance ((this rudel-beep-transport) slots)
  "Initialize THIS and register states."
  ;; Initialize slots of THIS.
  (when (next-method-p)
    (call-next-method))

  ;; Register states.
  (rudel-register-states this rudel-beep-states)

  ;;
  (with-slots (channels) this
    (setq channels (make-hash-table))
    (puthash
     0
     (rudel-beep-channel-zero
      "0"
      :transport this
      :start '(greeting
	       ("http://iana.org/beep/TLS"
		"http://www.codingmonkeys.de/BEEP/SubEthaEditHandshake"
		"http://www.codingmonkeys.de/BEEP/TCMMMStatus"
		"http://www.codingmonkeys.de/BEEP/SubEthaEditSession")))
     channels)) ;; TODO get these through an initarg

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

(defmethod rudel-dispatch-frame ((this rudel-beep-transport) frame)
  ""
  (with-slots (channels) this
    (with-slots ((frame-channel :channel)) frame
      (let ((channel (gethash frame-channel channels)))
	(rudel-accept channel frame)))))

(defmethod rudel-create-channel ((this rudel-beep-transport) id profiles)
  ""
  (with-slots (channels) this

    (rudel-switch
     (gethash 0 channels)
     'starting profiles)

    (rudel-state-wait (gethash 0 channels) '(established))

    (let ((channel (rudel-beep-channel
		    (format "%d" id)
		    :id id
		    :transport this
		    :start 'established)))
      (puthash id channel channels)
      channel))
  )


;;; Autoloading
;;

;;;###autoload
(rudel-backend-register 'transport 'beep 'rudel-beep-backend)

(provide 'rudel-beep)
;;; rudel-beep.el ends here
