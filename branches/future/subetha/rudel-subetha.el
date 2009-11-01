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
;; 0.1 - Initial revision


;;; Code:
;;

(eval-when-compile
  (require 'cl))

(require 'eieio)

(require 'rudel-backend)
(require 'rudel-util)

(defconst rudel-subetha-version '(0 1)
  "Version of the subetha backend for Rudel.")


;;;  Class rudel-subetha-backend
;;

(defclass rudel-subetha-backend (rudel-backend)
  ((capabilities :initform '(join host
			     change-color
			     chat
			     track-subscriptions
			     track-cursor)))
  "Class rudel-subetha-backend ")

(defmethod initialize-instance ((this rudel-subetha-backend) slots)
  ""
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-subetha-version))

(defmethod rudel-ask-connect-info ((this rudel-subetha-backend) &optional info)
  ""
  (let ((username (or (and info (plist-get info :username))
		      (read-string "Username: " rudel-default-username)))
	(color    (or (and info (plist-get info :color))
		      (read-color  "Color: " t))))
    (list :username username :color color)))

(defmethod rudel-connect ((this rudel-subetha-backend) transport info)
  "TODO"
  ;; Before we start, load the client functionality.
  (require 'rudel-subetha-client)

  ;;
  (rudel-subetha-client-connection "bla"))

(defmethod rudel-make-document ((this rudel-subetha-backend)
				name encoding session)
  "TODO")


;;; Autoloading
;;

;;;###autoload
(rudel-backend-register 'protocol 'subetha 'rudel-subetha-backend)

;;;###autoload
(eval-after-load 'rudel-zeroconf
  '(rudel-zeroconf-register-service "_see._tcp" 'tcp 'subetha))

(provide 'rudel-subetha)
;;; rudel-subetha.el ends here
