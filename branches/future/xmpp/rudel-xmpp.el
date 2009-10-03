;;; rudel-xmpp.el --- XMPP transport backend for Rudel
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, XMPP, transport, backend
;; X-RCS: $Id:$
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.


;;; Commentary:
;;


;;; History:
;;
;; 0.1 - initial version


;;; Code:
;;

(require 'rudel-backend)
(require 'rudel-transport)

(require 'rudel-util)

(require 'rudel-xmpp-state)
(require 'rudel-xmpp-socket-owner)


;;; Constants
;;

(defconst rudel-xmpp-protocol-version '(0 1)
  "Version of the XMPP backend for Rudel.")


;;; Class rudel-xmpp-backend
;;

(defclass rudel-xmpp-backend (rudel-transport-backend)
  ()
  "")

(defmethod initialize-instance ((this rudel-xmpp-backend) &rest slots)
  ""
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-xmpp-protocol-version))

(defmethod rudel-connect ((this rudel-xmpp-backend) info)
  "Connect to an XMPP server using the information in INFO.
INFO has to be a property list containing at least the keys :host
and :port."
  (let* ((host      (plist-get info :host))
	 (port      (plist-get info :port))
	 ;; Create the network process
	 ;; TODO do this in the transport's constructor?
	 (socket    (funcall
		     ;; (if encryption
		     ;;     (progn
		     ;;	(require 'rudel-tls)
		     ;;	#'rudel-tls-make-process)
		     #'make-network-process
		     :name     host
		     :host     host
		     :service  port
		     ;; Install connection filter to redirect data to
		     ;; the connection object
		     :filter   #'rudel-filter-dispatch
		     ;; Install connection sentinel to redirect state
		     ;; changes to the connection object
		     :sentinel #'rudel-sentinel-dispatch
		     ;; Do not start receiving immediately since the
		     ;; filter function is not yet setup properly.
		     :stop     t))
	 (transport (rudel-xmpp-transport
		     host
		     :socket socket)))

    ;; Now start receiving and wait until the connection has been
    ;; established.
    (continue-process socket)
    (rudel-state-wait transport
		      '(established) '(we-finalize they-finalize)
		      "Connecting")

    ;;
    transport))


;;; Class rudel-xmpp-state-new
;;

(defclass rudel-xmpp-state-new (rudel-xmpp-state)
  ()
  "")

(defmethod rudel-enter ((this rudel-xmpp-state-new))
  ""
  '(negotiate-stream sasl-start))


;;; Class rudel-xmpp-state-negotiate-stream
;;

(defclass rudel-xmpp-state-negotiate-stream (rudel-xmpp-state)
  ((success-state :initarg :success-state
		  :type    symbol
		  :documentation
		  ""))
  "")

(defmethod rudel-enter ((this rudel-xmpp-state-negotiate-stream)
			success-state) ;; host)
  ""
  ;; Store name of the successor state in case of successful stream
  ;; negotiation for later.
  (oset this :success-state success-state)

  ;; The first message we receive will be an incomplete <stream:stream
  ;; ... > XML tree.
  (rudel-set-assembly-function this #'rudel-xmpp-assemble-stream)

  ;; We cannot generate this message by serializing an XML infoset
  ;; since the document is incomplete. We construct it as a string
  ;; instead.
  (rudel-send this
	      (format "<?xml version=\"1.0\" encoding=\"%s\"?><stream:stream xmlns:stream=\"http://etherx.jabber.org/streams\" xmlns=\"jabber:client\" version=\"%s\" to=\"%s\" id=\"%s\">"
		      "UTF-8"
		      rudel-xmpp-protocol-version
		      "jabber.org" ;;"gunhead.local"
		      "scymtym@jabber.org")) ;; "361729367874")) ;; "gunhead")) ;; host))
  nil)

(defmethod rudel-leave ((this rudel-xmpp-state-negotiate-stream))
  ""
  ;; TODO explain
  (rudel-set-assembly-function this #'rudel-assemble-xml))

(defmethod rudel-accept ((this rudel-xmpp-state-negotiate-stream) xml)
  ""
  (cond
   ;; Stream negotiation error.
   ;;((string= (xml-tag-name xml) "stream:stream")
   ;;nil) ;; TODO send error

   ;; Success
   (t
    (with-slots (success-state) this
      (let ((features (xml-tag-children
		       (xml-tag-child xml "stream:features"))))
	(list success-state features)))))
  )


;;; Class rudel-xmpp-state-authenticated
;;

;; TODO similar to new state; could use common base class
(defclass rudel-xmpp-state-authenticated (rudel-xmpp-state)
  ()
  "")

(defmethod rudel-enter ((this rudel-xmpp-state-authenticated))
  ""
  (list 'negotiate-stream 'established))


;;; Class rudel-xmpp-state-authentication-failed
;;

(defclass rudel-xmpp-state-authentication-failed (rudel-xmpp-state)
  ()
  "")


;;; Class rudel-xmpp-state-established
;;

(defclass rudel-xmpp-state-established (rudel-xmpp-state)
  ()
  "")

(defmethod rudel-enter ((this rudel-xmpp-state-established) features)
  ""
  nil)

(defmethod rudel-accept ((this rudel-xmpp-state-established) xml)
  ""
  (with-slots (handler) this
    (when handler
      (funcall handler xml)))
  nil)


;;; Class rudel-xmpp-state-we-finalize
;;

(defclass rudel-xmpp-state-we-finalize (rudel-xmpp-state)
  ()
  "")

(defmethod rudel-enter ((this rudel-xmpp-state-we-finalize))
  ""
  (rudel-send this "</stream:stream>")

  ;; TODO (rudel-close connection))?
  'disconnected)


;;; Class rudel-xmpp-state-they-finalize
;;

(defclass rudel-xmpp-state-they-finalize (rudel-xmpp-state)
  ()
  "")

(defmethod rudel-enter ((this rudel-xmpp-state-they-finalize))
  ""
  (rudel-close this)
  nil)


;;; Class rudel-xmpp-state-disconnected
;;

(defclass rudel-xmpp-state-disconnected (rudel-xmpp-state)
  ()
  "")


;;; XMPP state list
;;

(defvar rudel-xmpp-states
  '(;; Basic XMPP states
    (new                   . rudel-xmpp-state-new)
    (negotiate-stream      . rudel-xmpp-state-negotiate-stream)
    (authenticated         . rudel-xmpp-state-authenticated)
    (authentication-failed . rudel-xmpp-state-authentication-failed)
    (established           . rudel-xmpp-state-established)
    (we-finalize           . rudel-xmpp-state-we-finalize)
    (they-finalize         . rudel-xmpp-state-they-finalize)
    (disconnected          . rudel-xmpp-state-disconnected))
  "")


;;; Class rudel-xmpp-transport
;;

(defclass rudel-xmpp-transport (rudel-state-machine
				rudel-xmpp-socket-owner
				rudel-transport)
  ((handler :initarg  :handler
	    :type     (or null function)
	    :initform nil
	    :documentation
	    ""))
  "")

(defmethod initialize-instance ((this rudel-xmpp-transport)
				&rest slots)
  "Initialize THIS and register states."
  ;; Initialize slots of THIS.
  (when (next-method-p)
    (call-next-method))

  ;; Register states.
  (rudel-register-states this rudel-xmpp-states)
  )

(defmethod rudel-register-state ((this rudel-xmpp-transport)
				 symbol state)
  "Associate THIS to STATE before registering STATE."
  ;; Associate THIS connection to STATE.
  (oset state :transport this)

  ;; Register the modified STATE.
  (call-next-method)
  )

(defmethod rudel-transport-set-handler ((this rudel-transport)
					 handler1)
  "TODO"
  (with-slots (handler) this
    (setq handler handler1)))

;; TODO I don't look this name too much
(defmethod rudel-transport-send ((this rudel-xmpp-transport) xml)
  ""
  (rudel-send this xml))

(defmethod rudel-message ((this rudel-xmpp-transport) xml)
  ""
  (rudel-accept this xml))

(defmethod rudel-disconnect ((this rudel-xmpp-transport))
  ""
  (rudel-switch this 'we-finalize)

  (rudel-state-wait this '(disconnected) "Disconnecting")

  (when (next-method-p)
    (call-next-method)) ;; TODO does this call rudel-close again?
  )


;;; Autoloading
;;

;;;###autoload
(rudel-add-backend (rudel-backend-get-factory 'transport)
		   'xmpp 'rudel-xmpp-backend)

(provide 'rudel-xmpp)


;;; Unit tests
;;

(when nil

  (let* ((socket    (make-network-process
		     :name      "bla"
		     ;; :host      "jabber.org"
		     ;; :service    5222
		     ;; :host      "localhost"
		      ;; :service    6523
		     :host "gunhead.local"
		     :service 5222
		     :filter   #'rudel-filter-dispatch
		     :sentinel #'rudel-sentinel-dispatch
		     :stop     t)))
    (setq transport (rudel-xmpp-transport
		     "bla"
		     :socket socket))
    (continue-process socket)

    (rudel-state-wait transport
		      '(established) '(we-finalize they-finalize)
		      "Connecting")

    (rudel-transport-send transport '(("iq" ("xmlns" . "blup")) "bla")))

  (rudel-close transport)

  )

(when nil

  (let* ((info           '(:host "jabber.org"
			   :port 5222
			   :host "gunhead.local"
			   :port 6523
			   ))
	 (xmpp-backend   (cdr (rudel-get-backend
			       (rudel-backend-get-factory 'transport)
			       'xmpp)))
	 (xmpp-transport (rudel-connect xmpp-backend info))))

  )

;;; rudel-xmpp.el ends here
