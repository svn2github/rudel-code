;;; rudel-beep-debug.el --- Debugging functions for the Rudel BEEP backend
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, beep, debug
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
;; Debugging functions for the Rudel BEEP transport backend.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'eieio)

(require 'rudel-beep-channel)


;;; BEEP states
;;

(defmethod rudel-debug-target ((this rudel-beep-state))
  "Return debug target of the transport as debug target for THIS."
  (with-slots (transport) this
    (rudel-debug-target transport)))


;;; BEEP channels
;;

(defmethod rudel-debug-target ((this rudel-beep-channel))
  "Return debug stream name for THIS."
  (format "channel %s" (object-name-string this)))

(provide 'rudel-beep-debug)
;;; rudel-beep-debug.el ends here
