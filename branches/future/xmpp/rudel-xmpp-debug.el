;;; rudel-xmpp-debug.el --- Debugging functions for the Rudel XMPP backend
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, debugging
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
;; along with rudel. If not, see <http://www.gnu.org/licenses>.


;;; Commentary:
;;
;; Debugging functions for Rudel XMPP transport backend.


;;; History:
;;
;; 0.1 - Initial revision


;;; Code:
;;

(require 'eieio)

(require 'rudel-xml)
(require 'rudel-debug)

(require 'rudel-xmpp-socket-owner)
(require 'rudel-xmpp)
(require 'rudel-xmpp-sasl)


;;; Variables
;;

(defvar rudel-xmpp-debug-old-state nil
  "TODO")


;;; Intercept rudel-xmpp-socket-owner methods for logging
;;

(defmethod rudel-set-assembly-function :before
  ((this rudel-xmpp-socket-owner) function)
  ""
  (with-slots (socket assembly-function) this
    (rudel-debug-stream-insert
     (rudel-debug-stream-name socket)
     :special
     (format "ASSEMBLE %s -> %s"
	     (symbol-name assembly-function)
	     (symbol-name function))))
  )

(defmethod rudel-set-parse-function :before
  ((this rudel-xmpp-socket-owner) function)
  ""
  (with-slots (socket assembly-function) this
    (rudel-debug-stream-insert
     (rudel-debug-stream-name socket)
     :special
     (format "PARSE %s -> %s"
	     (symbol-name assembly-function)
	     (symbol-name function))))
  )

(defmethod rudel-send :before ((this rudel-xmpp-socket-owner)
			       string-or-xml)
  ""
  (let ((formatted (if (stringp string-or-xml)
		       string-or-xml
		     (condition-case nil
			 (xml->string string-or-xml t)
		       (error (format "<could not format XML infoset>\n%s"
				      string-or-xml))))))
    (with-slots (socket) this
      (rudel-debug-stream-insert
       (rudel-debug-stream-name socket)
       :sent
       formatted (unless (stringp string-or-xml)
		   string-or-xml))))
  )

(defmethod rudel-message :before ((this rudel-xmpp-socket-owner)
				  xml)
  ""
  (with-slots (socket) this
    (let ((formatted (condition-case nil
			 (xml->string xml t)
		       (error (format "<could not format XML infoset>\n%s"
				      xml)))))
      (rudel-debug-stream-insert
       (rudel-debug-stream-name socket)
       :received
       formatted xml)))
  )

(defmethod rudel-switch :before ((this rudel-xmpp-socket-owner)
				 state &rest arguments)
  ""
  (with-slots (state) this
    (setq rudel-xmpp-debug-old-state
	  (if state
	      (object-name-string state)
	    "#start")))
  )

(defmethod rudel-switch :after ((this rudel-xmpp-socket-owner)
				state &rest arguments)
  ""
  (with-slots (socket state) this
    (let ((old-state rudel-xmpp-debug-old-state)
	  (new-state (object-name-string state)))
      (unless (string= old-state new-state)
	(rudel-debug-stream-insert
	 (rudel-debug-stream-name socket)
	 :special
	 (if arguments
	     (format "STATE %s -> %s %s" old-state new-state arguments)
	   (format "STATE %s -> %s" old-state new-state))))))
  )

(defmethod rudel-send :after ((this rudel-xmpp-state-sasl-mechanism-step)
			       xml)
  ""
  (when (and (string= (xml-tag-name xml) "response")
	     (car-safe (xml-tag-children xml)))
    (with-slots (socket) (oref this :transport) ;; TODO temp impersonating-state solves this
      (mapc
       (lambda (pair)
	 (rudel-debug-stream-insert
	  (rudel-debug-stream-name socket)
	  :sent
	  (if (find ?= pair)
	      (apply #'format "%-16s: %s" (split-string pair "="))
	    pair)))
       (split-string
	(base64-decode-string (car (xml-tag-children xml)))
	 ","))))
  )

(defmethod rudel-accept :before ((this rudel-xmpp-state-sasl-mechanism-step)
				 xml)
  ""
  (when (and (string= (xml-tag-name xml) "challenge")
	     (car-safe (xml-tag-children xml)))
    (with-slots (socket) (oref this :transport) ;; TODO temp impersonating-state solves this
      (mapc
       (lambda (pair)
	 (rudel-debug-stream-insert
	  (rudel-debug-stream-name socket)
	  :received
	  (if (find ?= pair)
	      (apply #'format "%-16s: %s" (split-string pair "="))
	    pair)))
       (split-string
	  (base64-decode-string (car (xml-tag-children xml)))
	  ","))))
  )

(provide 'rudel-xmpp-debug)
;;; rudel-xmpp-debug.el ends here
