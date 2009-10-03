;;; rudel-user.el --- Rudel user class
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, domain model, user
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
;; 0.2 - Hooks
;;
;; 0.1 - initial version


;;; Code:
;;

(require 'eieio)


;;; Class rudel-user
;;

(defclass rudel-user (eieio-named
		      rudel-hook-object) ;; TODO require providing library
  ((color       :initarg  :color
		:accessor rudel-color
		:documentation ;; type color or is it really string? or color-defined
		"Color used to indicate ownership or authorship
by the user. Examples includes text written by the user or the
user name itself.")
   ;; Hooks
   (change-hook :initarg  :change-hook
		:type     list
		:initform nil
		:documentation
		"This hook is run when this user object
changes."))
  "Objects of this class represent users participating in
collaborative editing session. Note that a participating user
does not have to be connected to the session at any given time."
  :abstract t)

(defmethod rudel-change-notify ((this rudel-user))
  "Run change hook of THIS after slot values have changed."
  (object-run-hook-with-args this 'change-hook))

(provide 'rudel-user)
;;; rudel-user.el ends here
