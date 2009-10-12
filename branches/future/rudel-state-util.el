;;; rudel-state-util.el --- Utility functions for state machine states
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, state, machine, utility
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
;; This file contains the mixins `rudel-impersonating-state' and
;; `rudel-delegating-state' which allow implementations of state
;; machine states to transparently access slots and methods of the
;; associated state machine.


;;; History:
;;
;; 0.1 - initial version


;;; Code:
;;

(require 'rudel-state-machine)


;;; Class rudel-impersonating-state
;;

(defclass rudel-impersonating-state (rudel-state) ;; TODO not sure whether this belongs here
  ((master-slot :type       symbol
		:allocation :class
		:documentation
		"A symbol specifying the name of the slot that
holds the reference to the state machine which own the state
object."))
  "A mixin that allows derived state classes to transparently
accesses the slots of their state machines."
  :abstract t)

(defmethod slot-missing ((this rudel-impersonating-state)
			 slot-name operation &optional new-value)
  "Look up SLOT-NAME in the state machine associated to THIS."
  (let ((master (slot-value this (oref this master-slot))))
    (case operation
      (oref
       (slot-value master slot-name))

      (oset
       (set-slot-value master slot-name new-value))))
  )

;; TODO we could use (defmethod no-applicable-method ((object) method &rest args)


;;; Class rudel-delegating-state
;;

(defclass rudel-delegating-state (rudel-state)
  ((socket-owner-slot :type       symbol
		      :allocation :class
		      :documentation
		      "A symbol specifying the name of the slot
that holds the reference to the state machine which own the state
object."))
  "A mixin that allows derived state classes to transparently
call certain methods of their state machines."
  :abstract t)

(defmethod rudel-send ((this rudel-delegating-state) data)
  "Call `rudel-send' of the state machine of THIS with DATA."
  (let ((master (slot-value this (oref this socket-owner-slot))))
    (rudel-send master data)))

(defmethod rudel-close ((this rudel-delegating-state))
  "Call `rudel-close' of the state machine of THIS"
  (let ((master (slot-value this (oref this socket-owner-slot))))
    (rudel-close master)))

(provide 'rudel-state-util)
;;; rudel-state-util.el ends here
