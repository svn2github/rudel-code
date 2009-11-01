;;; rudel-subetha-client.el --- 
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, subethaedit, backend, client
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

(require 'rudel-state-machine)


;;; Class rudel-subetha-client-connection-state-handshake
;;

(defclass rudel-subetha-client-connection-state-handshake ()
  ()
  "TODO")

(defmethod rudel-enter ((this rudel-subetha-client-connection-state-handshake))
  ""
  (rudel-create-channel
   1 '("http://www.codingmonkeys.de/BEEP/SubEthaEditHandshake"))

  nil)


;;; Class rudel-subetha-client-connection-state-opening
;;

(defclass rudel-subetha-client-connection-state-opening ()
  ()
  "TODO")

(defmethod rudel-enter ((this rudel-subetha-client-connection-state-opening))
  ""
  (with-slots (status-channel) this
    (setq status-channel
	  (rudel-create-channel
	   3 '("http://www.codingmonkeys.de/BEEP/TCMMMStatus")))))


;;; Class rudel-subetha-client-connection-state-established
;;

(defclass rudel-subetha-client-connection-state-established ()
  ()
  "TODO")


;;; Client connections states
;;

(defvar rudel-subetha-client-connection-states
  '((handshake   . rudel-subetha-client-connection-state-handshake)
    (opening     . rudel-subetha-client-connection-state-opening)
    (established . rudel-subetha-client-connection-state-established))
  "")


;;; Class rudel-subetha-client-connection
;;

(defclass rudel-subetha-client-connection (rudel-connection)
  ((status-channel   :initarg :status-channel
		     :type    rudel-beep-channel ;; TODO rudel-transport?
		     :documentation
		     "")
   (session-channels :initarg  :session-channels
		     :initform nil
		     :type     list
		     :documentation
		     ""))
  "Class rudel-subetha-client-connection ")

(provide 'rudel-subetha-client)
;;; rudel-subetha-client.el ends here
