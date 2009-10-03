;;; rudel-infinote-operations.el --- Operations on infinote documents
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, infinote, operations
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

(require 'eieio)


;;; Class rudel-infinote-cursor-reposition
;;

(defclass rudel-infinote-cursor-reposition (rudel-operation) ;; TODO does this have anything to do with infinote
  ((position :initarg :position
	     :type    (integer 0)
	     :documentation
	     ""))
  "Objects of this class reposition the visual indication of
another user's cursor when applied.")

(defmethod rudel-apply ((this rudel-infinote-cursor-reposition) object)
  ""
  (rudel-move-cursor object position))


;;; Class rudel-infinote-selection-reposition
;;

(defclass rudel-infinote-selection-reposition ()
  ((position :initarg :position
	     :type    (integer 0)
	     :documentation
	     "")
   (length   :initarg :length
	     :type    (integer 0)
	     :documentation
	     ""))
  "Objects of this class reposition the visual indication of
another user's selection when applied.")

(provide 'rudel-infinote-operations)
;;; rudel-infinote-operations.el ends here
