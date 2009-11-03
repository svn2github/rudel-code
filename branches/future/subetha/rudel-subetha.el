;;; rudel-subetha.el --- A subetha backend for Rudel
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, subethaedit, protocol, backend
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
;; 0.1 - Initial version


;;; Code:
;;

(eval-when-compile
  (require 'cl))

(require 'eieio)

(require 'rudel-backend)
(require 'rudel-protocol)
(require 'rudel-util)

(defconst rudel-subetha-version '(0 1)
  "Version of the subetha backend for Rudel.")


;;;  Class rudel-subetha-backend
;;

(defclass rudel-subetha-backend (rudel-protocol-backend)
  ((capabilities :initform '(join host
			     change-color
			     chat
			     track-subscriptions
			     track-cursor
			     user-icons)))
  "Class rudel-subetha-backend ")

(defmethod initialize-instance ((this rudel-subetha-backend) slots)
  ""
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-subetha-version))

(defmethod rudel-ask-connect-info ((this rudel-subetha-backend)
				   &optional info)
  ""
  (let ((username (or (and info (plist-get info :username))
		      (read-string "Username: " rudel-default-username)))
	(color    (or (and info (plist-get info :color))
		      (read-color  "Color: " t))))
    (list :username username :color color)))

(defmethod rudel-connect ((this rudel-subetha-backend) transport info
			  &optional progress-callback)
  "Connect to an SubEthaEdit server using the information INFO.
Return the connection object."
  ;; Before we start, load the client functionality.
  (require 'rudel-subetha-client)

  ;; Create the connection object.
  (let* ((host       (plist-get info :host))
	 (connection (rudel-subetha-client-connection
		      host
		      :transport transport)))

    ;; Start the transport and wait until the basic session setup is
    ;; complete.
    (rudel-start transport)

    (rudel-state-wait connection
		      '(established) nil
		      progress-callback)

    ;; The connection is now usable; return it.
    connection))

(defmethod rudel-make-document ((this rudel-subetha-backend)
				name encoding session)
  "TODO")


;;; Autoloading
;;

;;;###autoload
(rudel-backend-register 'protocol 'subetha 'rudel-subetha-backend)

;;;###autoload
(eval-after-load 'rudel-zeroconf
  '(rudel-zeroconf-register-service "_see._tcp" 'beep 'subetha))

(provide 'rudel-subetha)
;;; rudel-subetha.el ends here
