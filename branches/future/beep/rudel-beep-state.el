;;; rudel-beep-state.el --- Base class for states used in BEEP connections
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, beep, state machine
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
;; The class `rudel-beep-state' is the base class for all states used
;; in BEEP connections.


;;; History:
;;
;; 0.1 - Initial revision


;;; Code:
;;

(require 'rudel-state-machine)
(require 'rudel-state-util)


;;; Class rudel-beep-state
;;

(defclass rudel-beep-state (rudel-impersonating-state
			    rudel-delegating-state)
  ((impersonating-target-slot :initform 'transport)
   (delegation-target-slot    :initform 'transport)
   (transport                 :initarg  :transport
			      :type     rudel-transport ;; TODO rudel-beep-transport?
			      :documentation
			      "The transport class the state
machine of which uses the state object."))
  "Base class for BEEP state classes.")

(defmethod rudel-enter ((this rudel-beep-state))
  "Default behavior is to stay in the newly entered state."
  nil)

(defmethod rudel-leave ((this rudel-beep-state))
  "Default behavior is to do nothing when leaving a state.")

(defmethod rudel-accept ((this rudel-beep-state) frame)
  "Default behavior is to accept FRAME without taking action."
  nil)

(provide 'rudel-beep-state)
;;; rudel-beep-state.el ends here
