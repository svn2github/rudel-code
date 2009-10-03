;;; rudel-infinote-states.el --- Base classes for infinote states 
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, infinotes, states
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

(require 'rudel-state-machine)

(require 'rudel-infinote-util)


;;; Class rudel-infinote-state
;;

;; TODO server connections should use the same state class
(defclass rudel-infinote-state (rudel-state)
  ((connection :initarg :connection
	       :type    rudel-infinote-client-connection
	       :documentation
	       ""))
  "Base class for infinote state classes.")

(defmethod rudel-enter ((this rudel-infinote-state))
  ""
  nil)

(defmethod rudel-leave ((this rudel-infinote-state))
  "")

(defmethod rudel-accept ((this rudel-infinote-state) xml)
  ""
  nil)

;; 
;; ;;; Class rudel-infinote-state-in-xmpp
;; ;;
;;
;; (defclass rudel-infinote-state-in-xmpp (rudel-infinote-state) ;; TODO move to XMPP backend
;;   ()
;;   "")
;;
;; (defmethod rudel-accept ((this rudel-infinote-state-in-xmpp) xml)
;;   ""
;;   (let ((name (xml-tag-name xml)))
;;     (cond
;;      ;;
;;      ;; TODO example
;;      ;; <stream:error>
;;      ;; <not-authorized xmlns="urn:ietf:params:xml:ns:xmpp-streams"/>
;;      ;; </stream:error>
;;      ("stream:error" 'they-finalize)
;;
;;      ;; we do not accept unexpected messages.
;;      (t 'we-finalize)))
;;   )


;;; Class rudel-infinote-state-in-group
;;

(require 'xml-parse) ;; TODO

;; TODO rudel-infinote-group-state?
(defclass rudel-infinote-state-in-group (rudel-infinote-state) ;; (rudel-infinote-state-in-xmpp)
  ()
  "")

(defmethod rudel-accept ((this rudel-infinote-state-in-group) xml)
  ""
  (let ((name (xml-tag-name xml)))
    (cond
     ;;
     ((string= name "group")
      (let* ((group-name (xml-tag-attr xml "name"))
	     (child      (nth 0 (xml-tag-children xml))) ;; TODO is there a better way?
	     (type       (xml-tag-name child)))
	(cond
	 ;;
	 ((string= type "request-failed")
	  'established)

	 ;; dispatch
	 (t (rudel-dispatch this type (list child))))))

     ;;
     (t (call-next-method))))
  )

(provide 'rudel-infinote-states)
;;; rudel-infinote-states.el ends here
