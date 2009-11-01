;;; rudel-subetha-client.el --- Client part of the Rudel SubEthaEdit backend
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
;; This file contains the client part of the Rudel SubEthaEdit
;; backend.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel) ;; for `rudel-connection'

(require 'rudel-state-machine)

(require 'rudel-subetha-constants)


;;; Class rudel-subetha-client-connection-state
;;

(defclass rudel-subetha-client-connection-state ;; TODO common base class for server and client?
  (rudel-impersonating-state
   rudel-delegating-state)
  ((connection                :initarg :connection
			      ;; TODO :type    rudel-subetha-client-connection
			      :documentation
			      "")
   (impersonating-target-slot :initform 'connection)
   (delegation-target-slot    :initform 'connection))
  "Base class for connection state classes.")

(defmethod rudel-enter ((this rudel-subetha-client-connection-state))
  "Default behavior is to remain in state THIS without further action."
  nil)

(defmethod rudel-leave ((this rudel-subetha-client-connection-state))
  "Default behavior is to not take any action.")


;;; Class rudel-subetha-client-connection-state-handshake
;;

(defclass rudel-subetha-client-connection-state-handshake
  (rudel-subetha-client-connection-state)
  ()
  "TODO")

(defmethod rudel-enter ((this rudel-subetha-client-connection-state-handshake))
  ""
  ;; Create a channel for the handshake procedure and attach a
  ;; handshake protocol object to it.
  (let* ((channel  (rudel-create-channel
		    this (list rudel-subetha-handshake-profile)))
	 (protocol (rudel-subetha-channel-protocol-handshake
		    "handshake"
		    :channel channel
		    :start   '(start "see://codingmonkeys.de:6742"))))) ;; TODO
  'opening-status)


;;; Class rudel-subetha-client-connection-state-opening-status
;;

(defclass rudel-subetha-client-connection-state-opening-status
  (rudel-subetha-client-connection-state)
  ()
  "TODO")

(defmethod rudel-enter
  ((this rudel-subetha-client-connection-state-opening-status))
  ""
  (with-slots (status-channel) this
    (setq status-channel (rudel-create-channel
			  this (list rudel-subetha-status-profile)))

    (rudel-subetha-channel-protocol-status
     "status"
     :channel status-channel
     :start 'idle))

  'established)


;;; Class rudel-subetha-client-connection-state-established
;;

(defclass rudel-subetha-client-connection-state-established
  (rudel-subetha-client-connection-state)
  ()
  "TODO")


;;; Client connections states
;;

(defvar rudel-subetha-client-connection-states
  '((handshake      . rudel-subetha-client-connection-state-handshake)
    (opening-status . rudel-subetha-client-connection-state-opening-status)
    (established    . rudel-subetha-client-connection-state-established))
  "")


;;; Class rudel-subetha-client-connection
;;

(defclass rudel-subetha-client-connection (rudel-state-machine
					   rudel-connection)
  ((transport        :initarg :transport
		     :type    rudel-transport-child
		     :documentation
		     "") ;; TODO maybe this belongs in the base class?
   (status-channel   :initarg  :status-channel
		     :type     rudel-beep-channel ;; TODO rudel-transport?
		     :documentation
		     "TODO")
   (session-channels :initarg  :session-channels
		     :initform nil
		     :type     list
		     :documentation
		     "TODO")
   (next-channel-id  :initarg  :next-channel-id
		     :type     (integer 0)
		     :initform 1
		     :documentation
		     ""))
  "Class rudel-subetha-client-connection ")

(defmethod initialize-instance ((this rudel-subetha-client-connection) slots)
  ""
  ;; Initialize slots of THIS.
  (when (next-method-p)
    (call-next-method))

  (rudel-register-states this rudel-subetha-client-connection-states))

(defmethod rudel-register-state ((this rudel-subetha-client-connection)
				 symbol state)
  ""
  "Associate THIS to STATE before registering STATE."
  ;; Associate THIS connection to STATE.
  (oset state :connection this)

  ;; Register the modified STATE.
  (when (next-method-p)
    (call-next-method))
  )

(defmethod rudel-create-channel ((this rudel-subetha-client-connection)
				 profiles)
  ""
  (with-slots (transport) this
    (let ((id (rudel-next-channel-id this)))
      (rudel-create-channel transport id profiles))))

(defmethod rudel-next-channel-id ((this rudel-subetha-client-connection))
  "Return a channel id that is  unused within THIS."
  (with-slots (next-channel-id) this
    (prog1
	next-channel-id
      (incf next-channel-id 2))))

(provide 'rudel-subetha-client)
;;; rudel-subetha-client.el ends here