;;; rudel-xmpp-socket-owner.el --- Socket owner class for the XMPP backend
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: xmpp, backend, socket, Rudel
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
;; The class `rudel-xmpp-socket-owner' extends the
;; `rudel-assembling-socket-owner' class by installing suitable
;; assembly and parse functions (see `rudel-assembling-socket-owner'
;; for details) and serializing XML data when necessary.


;;; History:
;;
;; 0.1 - Initial revision


;;; Code:
;;

(require 'rudel-xml)
;; TODO (require 'rudel-socket-owner)


;;; Fragmentation and assembling functions.
;;

(defun rudel-xmpp-assemble-stream (data)
  "Extract complete XML stanzas from DATA.
Return a list the first element of which is a list of strings
that contain the serialized forms of the stanzas. The second
element is a string containing the rest of the data or nil DATA
does not contains any incomplete stanzas."
  ;; TODO mention limitations
  (if (not (or (search "</stream:features>" data)
	       (search "<stream:features/>" data)))
      (list nil data)
    (destructuring-bind (tags buffer)
	(rudel-xml-toplevel-tags (concat data "</stream:stream>"))
      (list
       (remove-if
	(lambda (tag)
	  (= (aref tag 1) ??))
	tags)
       buffer))) ;; TODO wrong
  )
;; One problem: peer can send
;; <stream:stream xmlns:stream="http://etherx.jabber.org/streams"
;;                xmlns="jabber:client"
;;                version="1.0"
;;                from="gunhead">
;; and then
;; <stream:features/>
;; we cannot assemble this properly; look into the RFC


;;; Class rudel-xmpp-socket-owner
;;

(defclass rudel-xmpp-socket-owner (rudel-assembling-socket-owner)
  ((assembly-function :initform 'rudel-xmpp-assemble-stream)
   (parse-function    :initform 'string->xml))
  "A socket owner that specializes in sending and receiving XMPP
messages."
  :abstract t)

(defmethod rudel-send ((this rudel-xmpp-socket-owner) string-or-xml)
  "Send STRING-OR-XML through the socket of THIS.
STRING-OR-XML can be of type string or an XML tree in which case
it will be serialised and send."
  (with-slots (socket) this
    (process-send-string socket (if (stringp string-or-xml)
				    string-or-xml
				  (xml->string string-or-xml)))) ;; TODO call-next-method?
  )

(provide 'rudel-xmpp-socket-owner)
;;; rudel-xmpp-socket-owner.el ends here
