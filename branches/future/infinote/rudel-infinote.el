;;; rudel-infinote.el --- Infinote backend for Rudel
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, gobby, infinote protocol
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
;; This file contains a Rudel protocol backend, which implements the
;; infinote protocol (used by the Gobby family of collaborative
;; editors starting with version 0.5).



;;; History:
;;
;; 0.2 - Support for transports
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel-backend)
(require 'rudel-protocol)


;;; Constants
;;

(defconst rudel-infinote-version '(0 2)
  "Version of the infinote backend for Rudel.")


;;; Class rudel-infinote-backend
;;

;;;###autoload
(defclass rudel-infinote-backend (rudel-protocol-backend)
  ((capabilities :initform '(join host
			     change-color
			     chat
			     track-subscriptions track-cursors
			     track-selections track-viewports
			     group-undo)))
  "")

(defmethod initialize-instance ((this rudel-infinote-backend) slots)
  ""
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-infinote-version))

(defmethod rudel-ask-connect-info ((this rudel-infinote-backend))
  ""
  ;; Obtain server host and port.
  (let ((host (read-string "Server: "))
	(port (read-number "Port: " 6524)))
    ;; Read desired username and color regardless of how the server
    ;; info was obtained.
    ;; (let ((username (rudel-read-user-name))
    ;;	  (color    (rudel-read-user-color)))
      ;; TODO something like rudel-read-color which can be customized to only offer very bright colors
      (list :host host
	    :port port))
	    ;; :username username
	    ;; :color color)))
  )

(defmethod rudel-connect ((this rudel-infinote-backend) transport info
			  &optional progress-callback)
  "Connect to an infinote server using the information INFO.
Return the connection object."
  ;; Before we start, load the client functionality.
  (require 'rudel-infinote-client)

  ;; Create the network process
  (let* ((session    (plist-get info :session))
	 (host       (plist-get info :host)) ;; Just as name
	 (connection (rudel-infinote-client-connection
		      (format "to %s" host)
		      :session   session
		      :transport transport)))

    ;; Start the transport and wait for it to establish the
    ;; connection.
    ;; TODO the waiting code is specific to the XMPP backend. avoid
    ;; this coupling
    (rudel-start transport)

    (rudel-state-wait transport
		      '(established) '(we-finalize they-finalize)
		      progress-callback)

    ;; The connection is now ready for action; Return it.
    connection)
  )

(defmethod rudel-ask-host-info ((this rudel-infinote-backend))
  ""
  (list :backend 'infinote
	:port    6524))

(defmethod rudel-host ((this rudel-infinote-backend) info)
  ""
  ;;
  (require 'rudel-infinote-server)

  ;; Make a server socket
  ;; TODO do we have to stop this process like in rudel-connect?
  (let* ((port   (plist-get info :port))
	 (socket (make-network-process ;; TODO do this in the constructor?
		  :name     (format "infinote-server on port %d"
				    port)
		  :host     "0.0.0.0" ;; TODO
		  :service  port
		  :server   t
		  :filter   #'rudel-filter-dispatch
		  :sentinel #'rudel-sentinel-dispatch
		  ;;
		  :log
		  (lambda (server-process client-process message) ;; TODO should be done by the server
		    (let ((server (rudel-process-object server-process)))
		      (rudel-add-client server client-process)))))
	 (server (rudel-infinote-server
		  "infinote-server" ;; TODO unique name
		  :backend this
		  :socket  socket)))
    server)
  )

(defmethod rudel-make-document ((this rudel-infinote-backend) ;; TODO maybe this should be in the session?
				name encoding session)
  ""
  (rudel-infinote-text-document name
				:session session)
  )


;;; Autoloading
;;

;;;###autoload
(rudel-backend-register 'protocol 'infinote 'rudel-infinote-backend)

;;;###autoload
(eval-after-load 'rudel-zeroconf
  '(rudel-zeroconf-register-service "_infinote._tcp" 'xmpp 'infinote))

(provide 'rudel-infinote)
;;; rudel-infinote.el ends here
