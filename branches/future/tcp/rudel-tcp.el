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


;;; Class rudel-tcp-backend
;;

(defclass rudel-tcp-backend (rudel-transport-backend)
  ()
  "TCP transport backend.
The transport backend is a factory for TCP transport objects.")

(defmethod initialize-instance ((this rudel-tcp-backend) &rest slots)
  ""
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-tcp-transport-version))

(defmethod rudel-make-connection ((this rudel-tcp-backend) info)
  "Connect to a TCP server using the information in INFO.
INFO has to be a property list containing the keys :host
and :port."
  (let* ((host      (plist-get info :host))
	 (port      (plist-get info :port))
	 ;; Create the network process
	 (socket    (#'make-network-process
		     :name     host
		     :host     host
		     :service  port
		     ;; Install connection filter to redirect data to
		     ;; the connection object
		     :filter   #'rudel-filter-dispatch
		     ;; Install connection sentinel to redirect state
		     ;; changes to the connection object
		     :sentinel #'rudel-sentinel-dispatch))
	 (transport (rudel-tcp-transport
		     host
		     :socket socket))))
  )


;;; Class rudel-tcp-transport
;;

(defclass rudel-tcp-transport (rudel-socket-owner
			       rudel-transport)
  ((handler :initarg  :handler
	    :type     (or null function)
	    :initform nil
	    :documentation
	    ""))
  "TCP transport.")

(defmethod rudel-set-handler ((this rudel-transport) handler1)
  "Install HANDLER1 as dispatcher for messages received by THIS."
  (with-slots (handler) this
    (setq handler handler1)))

;; TODO I don't like this name too much
(defmethod rudel-transport-send ((this rudel-tcp-transport) data)
  ""
  (rudel-send this data))

(defmethod rudel-receive ((this rudel-tcp-transport) data)
  (with-slots (handler) this
    (when handler
      (funcall hander data))))


;;; Autoloading
;;

;;;###autoload
(rudel-add-backend (rudel-backend-get-factory 'transport)
		   'tcp 'rudel-tcp-backend)

(provide 'rudel-tcp)
;;; rudel-tcp.el ends here
