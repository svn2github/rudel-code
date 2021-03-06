;;; rudel-infinote-group.el --- Common aspects of infinote communication groups
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, infinote, group, communication
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
;; This file contains the classes `rudel-infinote-group' which is a
;; base class for other infinote group classes and
;; `rudel-infinote-group-state' which is a base class for infinote
;; group state classes.


;;; History:
;;
;; 0.1 - Initial revision


;;; Code:
;;

(require 'eieio)
(require 'eieio-base)

(require 'rudel-state-machine)
(require 'rudel-infinote-states)


;;; Class rudel-infinote-group-state
;;

(defclass rudel-infinote-group-state (rudel-infinote-state)
  ((group :initarg :group
	  :type    rudel-infinote-group-child
	  :documentation
	  ""))
  ""
  :abstract t)

(defmethod rudel-accept ((this rudel-infinote-group-state) xml) ;; TODO is xml a single element or a list of elements?
  ""
  (let ((type (xml-tag-name xml)))
    (cond
     ;;
     ;;<request-failed domain="error_domain" code="error_code" seq="seq_id">
     ;; <text>Human-readable text</text>
     ;;</request-failed>
     ;; domain example: INF_DIRECTORY_ERROR
     ((string= type "request-failed")
      ;; TODO handle the problem
      (with-tag-attrs (domain
		       (code            code number)
		       (sequence-number seq  number)) xml
	)
      'idle)

     ;; Dispatch all normal message to appropriate methods
     ;; automatically.
     (t (rudel-dispatch this "rudel-infinote/" type (list xml)))))
  )

;; TODO can all groups receive <session-close/> or just document groups?

(defmethod rudel-send ((this rudel-infinote-group-state) data)
  ""
  (with-slots (group) this
    ;;
    ;(with-slots (sequence-number) connection ;; TODO encapsualtion violation
    (rudel-send group data))
  )


;;; Class rudel-infinote-group
;;

(defclass rudel-infinote-group (eieio-named
				rudel-state-machine)
  ((connection :initarg :connection
	       ;:type    rudel-infinote-connection ;; TODO
	       :documentation
	       "")
   (publisher  :initarg  :publisher
	       :type     string
	       :documentation
	       "")
   (method     :initarg  :method
	       :type     symbol
	       :documentation
	       "")
   (members    :initarg  :members ;; TODO currently unused
	       :type     list
	       :initform nil
	       :documentation
	       ""))
  "")

;; TODO we could introduce rudel-message to pass data to rudel accept

(defmethod rudel-register-state ((this rudel-infinote-group) symbol state)
  "TODO"
  ;; Associate THIS connection to STATE.
  (oset state :group this)

  ;;
  (call-next-method))

(defmethod rudel-send ((this rudel-infinote-group) data)
  ""
  (with-slots (connection) this
    (rudel-send connection
		(rudel-infinote-embed-in-group this data))))


;;; Miscellaneous functions
;;

;; TODO move to util file
(defmacro rudel-infinote-embed-in-group (group &rest forms);; TODO bad name
  ""
  (declare (indent 1)
	   (debug (form &rest form)))
  (let ((group-var (make-symbol "group"))
	(name      (make-symbol "name"))
	(publisher (make-symbol "publisher")))
    `(let* ((,group-var ,group)
	    (,name      (object-name-string ,group-var))
	    (,publisher (oref ,group-var :publisher)))
       `(("group"
	  ("name"      . ,,name)
	  ("publisher" . ,,publisher))
	 ,,@forms)))
  )

(provide 'rudel-infinote-group)


;;; Unit tests
;;

(eval-when-compile

  (require 'ert)

  (defclass rudel-infinote-test-group-state (rudel-infinote-group-state)
    ())

  (ert-deftest rudel-infinote-test-embed-in-group ()
    (let* ((state (rudel-infinote-test-group-state "test-state"))
	   (group (rudel-infinote-group
		  "test-group"
		  :publisher "test-publisher"
		  :states    `(test . state)
		  :start     state)))
      (should
       (equal
	(rudel-infinote-embed-in-group group
	  '(("bla" ("a" . "1"))))
	'(("group"
	   ("name" .      "test-group")
	   ("publisher" . "test-publisher"))
	  (("bla" ("a" . "1")))))))
    )

  )

;;; rudel-infinote-group.el ends here
