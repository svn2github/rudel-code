;;; rudel-subetha-channel-protocol.el --- Channel protocol support classes
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, subethaedit, protocol, backend, channel
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
;; This file contains the class `rudel-subetha-channel-protocol-state'
;; which is a base class for classes that implement protocols
;; associated with BEEP channel profiles.
;;
;; TODO This whole file could also belong in the BEEP backend


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel-state-util)


;;; Class rudel-subetha-channel-protocol
;;

(defclass rudel-subetha-channel-protocol (rudel-state-machine)
  ((channel :initarg :channel
	    :type    rudel-transport-child
	    :documentation
	    "The channel object on which the protocol exchanges
happen."))
  "Base class for channel protocols.
Objects of channel protocol classes are attached to channels to
realize stateful protocols on top of channels.")

(defmethod initialize-instance ((this rudel-subetha-channel-protocol) slots)
  "Initialize slots and connect `rudel-accept' as filter of the channel."
  ;; Initialize slots of THIS.
  (when (next-method-p)
    (call-next-method))

  ;; Connect `rudel-accept' as filter of the channel and use incoming
  ;; data as state machine input.
  (with-slots (channel) this
    (lexical-let ((this1 this))
      (rudel-set-filter channel
			(lambda (data)
			  (rudel-accept this1 data)))))
  )

(defmethod rudel-register-state ((this rudel-subetha-channel-protocol)
				 symbol state)
  "Associate THIS to STATE before registering STATE."
  ;; Associate THIS connection to STATE.
  (oset state :protocol this)

  ;; Register the modified STATE.
  (when (next-method-p)
    (call-next-method)))


;;; Class rudel-subetha-channel-protocol-state
;;

(defclass rudel-subetha-channel-protocol-state
  (rudel-impersonating-state
   rudel-delegating-state)
  ((protocol                  :initarg :protocol
			      :type    rudel-subetha-channel-protocol
			      :documentation
			      "The protocol object to which this
state is associated.")
   (impersonating-target-slot :initform 'protocol)
   (delegation-target-slot    :initform 'protocol))
  "Base class for state classes used in channel protocol
states.")

(defmethod rudel-enter ((this rudel-subetha-channel-protocol-state))
  "Default behavior is to remain in state THIS without further action."
  nil)

(defmethod rudel-leave ((this rudel-subetha-channel-protocol-state))
  "Default behavior is to not take any action.")

(provide 'rudel-subetha-channel-protocol)
;;; rudel-subetha-channel-protocol.el ends here
