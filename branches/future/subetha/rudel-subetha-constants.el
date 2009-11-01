;;; rudel-subetha-constants.el --- Constants used in the SubEthaEdit backend
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, subethaedit, constants
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
;; Some BEEP profile names used in the SubEthaEdit protocol.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(defconst rudel-subetha-handshake-profile
  "http://www.codingmonkeys.de/BEEP/SubEthaEditHandshake"
  "Profile URL identifying the handshake channel protocol used in
the SubEhtaEdit protocol.")

(defconst rudel-subetha-status-profile
  "http://www.codingmonkeys.de/BEEP/TCMMMStatus"
  "Profile URL identifying the status channel protocol used in
the SubEhtaEdit protocol.")

(defconst rudel-subetha-session-profile
  "http://www.codingmonkeys.de/BEEP/SubEthaEditSession"
  "Profile URL identifying the session channel protocol used in
the SubEhtaEdit protocol.")

(provide 'rudel-subetha-constants)
;;; rudel-subetha-constants.el ends here
