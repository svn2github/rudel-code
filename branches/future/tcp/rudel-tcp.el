;;; rudel-tcp.el --- TCP transport backend for Rudel
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, tcp, transport, backend
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
;; TCP transport backend for Rudel.


;;; History:
;;
;; 0.1 - initial version


;;; Code:
;;

(require 'rudel-backend)
(require 'rudel-transport)

(require 'rudel-util)


;;; Constants
;;

(defconst rudel-tcp-transport-version '(0 1)
  "Version of the TCP transport backend for Rudel.")


;;; Class rudel-tcp-transport
;;

;; TODO rudel-socket-transport?
(defclass rudel-tcp-transport (rudel-socket-owner
			       rudel-transport)
  ((filter   :initarg  :filter
	     :type     (or null function)
	     :initform nil
	     :accessor rudel-filter
	     :documentation
	     "")
   (sentinel :initarg  :sentinel
	     :type     (or null function)
	     :initform nil
	     :accessor rudel-sentinel
	     :documentation
	     ""))
  "TCP transport.")

(defmethod rudel-set-filter ((this rudel-tcp-transport) filter)
  "Install FILTER as dispatcher for messages received by THIS."
  (oset this :filter filter))

(defmethod rudel-set-sentinel ((this rudel-tcp-transport) sentinel)
  ""
  (oset this :sentinel sentinel))

(defmethod rudel-send ((this rudel-tcp-transport) data)
  ""
  (with-slots (socket) this
    ;;(rudel-send socket data)
    (process-send-string socket data)
    ))

(defmethod rudel-close ((this rudel-tcp-transport))
  ""
  )

(defmethod rudel-start ((this rudel-tcp-transport))
  (with-slots (socket) this
    (continue-process socket)))

(defmethod rudel-receive ((this rudel-tcp-transport) data)
  (with-slots (filter) this
    (when filter
      (funcall filter data))))


;;; Class rudel-tcp-backend
;;

(defclass rudel-tcp-backend (rudel-transport-backend)
  ((capabilities :initform (listen connect)))
  "TCP transport backend.
The transport backend is a factory for TCP transport objects.")

(defmethod initialize-instance ((this rudel-tcp-backend) &rest slots)
  ""
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-tcp-transport-version))

(defmethod rudel-make-connection ((this rudel-tcp-backend)
				  info info-callback
				  &optional progress-callback)
  "Connect to a TCP server using the information in INFO.
INFO has to be a property list containing the keys :host
and :port."
  ;; Ensure that INFO contains all necessary information.
  (unless (every (lambda (keyword) (member keyword info))
		 '(:host :port))
    (setq info (funcall info-callback this info)))

  ;; Extract information from INFO and create the socket.
  (let* ((host      (plist-get info :host))
	 (port      (plist-get info :port))
	 ;; Create the network process
	 (socket    (make-network-process
		     :name     host
		     :host     host
		     :service  port
		     ;; Install connection filter to redirect data to
		     ;; the connection object
		     :filter   #'rudel-filter-dispatch
		     ;; Install connection sentinel to redirect state
		     ;; changes to the connection object
		     :sentinel #'rudel-sentinel-dispatch
		     :stop     t))
	 (transport (rudel-tcp-transport
		     (format "to %s:%s" host port)
		     :socket socket)))
    transport))


;;; Autoloading
;;

;;;###autoload
(rudel-backend-register 'transport 'tcp 'rudel-tcp-backend)

(provide 'rudel-tcp)
;;; rudel-tcp.el ends here
