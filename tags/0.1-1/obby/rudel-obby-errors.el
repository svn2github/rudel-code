;;; rudel-obby-errors.el --- Error data used in the obby Rudel backend
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, obby, errors
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
;; This file contains definitions of error conditions and numeric
;; error codes used in the Rudel obby backend.


;;; History:
;;
;; 0.1 - Initial revision.


;;; Code:
;;


;;; Obby protocol error codes
;;

(defconst rudel-obby-error-username-invalid #x0001
  "Error code for invalid username.")

(defconst rudel-obby-error-username-in-use #x0002
  "Error code for username already in use.")

(defconst rudel-obby-error-color-in-use #x0100
  "Error code for color already in use.")

(provide 'rudel-obby-errors)
;;; rudel-obby-errors.el ends here
