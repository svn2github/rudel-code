;;; rudel-obby-util.el --- Miscellaneous functions for the Rudel XMPP backend
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, xmpp, backend, miscellaneous
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
;;


;;; History:
;;
;; 0.1 - Initial revision


;;; Code:
;;

(require 'rudel-xml)

(require 'rudel-transport-util)

(require 'rudel-xmpp-socket-owner) ;; TODO temp for rudel-xmpp-assemble-stream

(defun rudel-xmpp-make-transport-filter-stack (transport)
  "Construct an XMPP protocol filter stack on top of TRANSPORT."
  (rudel-transport-make-filter-stack
   transport
   '((rudel-assembling-transport-filter
      :assembly-function rudel-xmpp-assemble-stream)
     (rudel-parsing-transport-filter
      :parse-function    string->xml
      :generate-function xml->string))))

(provide 'rudel-xmpp-util)

;; TODO Unit tests

;;; rudel-xmpp-util.el ends here
