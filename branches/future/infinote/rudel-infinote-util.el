;;; rudel-infinote-util.el --- Miscellaneous functions for infinote backend
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, infinote, miscellaneous, utility
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
;; This file contains miscellaneous functions used in the infinote
;; backend.


;;; History:
;;
;; 0.1 - Initial revision


;;; Code:
;;
(eval-when-compile
  (require 'cl))

(require 'xml-parse)

(require 'rudel-util)
(require 'rudel-xml)

(require 'adopted-insert)
(require 'adopted-delete)
(require 'adopted-compound)
(require 'adopted-nop)


;;; Class rudel-infinote-socket-owner
;;

(defclass rudel-infinote-socket-owner (rudel-assembling-socket-owner)
  ((assembly-function :initform 'rudel-assemble-stream))
  ""
  :abstract t)

(defmethod rudel-send ((this rudel-infinote-socket-owner) string-or-xml)
  ""
  (with-slots (socket) this
    (process-send-string socket (if (stringp string-or-xml)
				    string-or-xml
				  (xml->string string-or-xml))))
  )


;;; Message serialization
;;

(defgeneric rudel-operation->xml ((this adopted-operation))
  "Generate an XML infoset from THIS operation.")

(defmethod rudel-operation->xml ((this adopted-insert))
  "Serialize THIS insert operation."
  (with-slots (from data) this
    `(("insert" ("pos" . ,(format "%d" from)))
      , data)))

(defmethod rudel-operation->xml ((this adopted-delete))
  "Serialize THIS delete operation."
  (with-slots (from length) this
    `(("delete"
       ("pos" . ,(format "%d" from))
       ("len" . ,(format "%d" length)))))
  )

(defmethod rudel-operation->xml ((this adopted-compound))
  "Serialize THIS compound operation."
  (with-slots (children) this
    (apply #'append
	   (list "split" )
	   (mapcar #'rudel-operation->xml children))))

(defmethod rudel-operation->xml ((this adopted-nop))
  "Serialize THIS nop operation."
  `(("nop")))


;;; Miscellaneous functions
;;

(defmacro rudel-infinote-embed-in-request (user &rest forms)
  ""
  (declare (indent 1)
	   (debug (form &rest form)))
  (let ((user-var (make-symbol "user"))
	(id-var   (make-symbol "id")))
    `(let ((,user-var ,user))
       (with-slots (,id-var) ,user-var
	 `(("request"
	    ("user" . ,(format "%d" ,id-var)))
	   ,,@forms))))
  )

(provide 'rudel-infinote-util)


;;; Unit tests
;;

;; (let ((data "<?xml version=\"1.0\"?><stream:stream xmlns:stream=\"http://etherx.jabber.org/streams\" xmlns=\"jabber:client\" version=\"1.0\" from=\"gunhead\"><stream:features><mechanisms xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\"><mechanism>ANONYMOUS</mechanism></mechanisms></stream:features>")
;;       (buffer))
;;   (rudel-assemble-stream-fragments data buffer)
;;   (list data buffer))

;(let ((tag '(("bla" (("a" . 1) ("b" . 2) ("c" . 3))))))
;  (pp (macroexpand '(with-tag-attrs (a b (c d)) taga nil)) #'insert))

;;; rudel-infinote-util.el ends here
