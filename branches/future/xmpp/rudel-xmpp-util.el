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
;; This file contains the assemble function
;; `rudel-xmpp-assemble-stream' which is used by
;; `rudel-xmpp-make-transport-filter-stack' to parametrize assembling
;; and parsing transport filters to handle XML data transparently.


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel-xml)

(require 'rudel-transport-util)


;;; Fragmentation and assembling functions.
;;

(defun rudel-xmpp-assemble-stream (data)
  "Extract complete XML stanzas from DATA.
Return a list the first element of which is a list of strings
that contain the serialized forms of the stanzas. The second
element is a string containing the rest of the data or nil DATA
does not contains any incomplete stanzas."
  ;; TODO mention limitations
  (let ((string (concat (nth 1 data) (nth 0 data))))
    (if (not (or (search "</stream:features>" string)
		 (search "<stream:features/>" string)))
	(list nil (list string))
      (destructuring-bind (tags buffer)
	  (rudel-xml-toplevel-tags (concat string "</stream:stream>"))
	(list
	 (remove-if
	  (lambda (tag)
	    (= (aref tag 1) ??))
	  tags)
	 (list buffer))))) ;; TODO wrong
  )
;; One problem: peer can send
;; <stream:stream xmlns:stream="http://etherx.jabber.org/streams"
;;                xmlns="jabber:client"
;;                version="1.0"
;;                from="gunhead">
;; and then
;; <stream:features/>
;; we cannot assemble this properly; look into the RFC


;;; Transport filter stack construction
;;

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
