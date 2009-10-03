;;; rudel-xmpp-tls.el --- TLS support for XMPP connections
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: XMPP, TLS, encryption, Rudel
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
;; 0.1 - initial revision


;;; Code:
;;

(require 'rudel-tls)

(require 'rudel-xmpp)


;;;
;;


;;; TLS state list
;;

(setq rudel-xmpp-states
      (append
       rudel-xmpp-states
       '(;; TLS states
	 start-tls . rudel-xmlpp-start-tls-start)))

(provide 'rudel-xmpp-tls)
;;; rudel-xmpp-starttls.el ends here
