;;; rudel-infinote-client.el --- Client part of the infinote backend for Rudel
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, infinote, client
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
;; This file contains the client part of the infinote backend for
;; Rudel,


;;; History:
;;
;; 0.1 - Initial revision


;;; Code:
;;

(require 'xml-parse)

(require 'rudel)
(require 'rudel-state-machine) ;; TODO necessary?
(require 'rudel-infinote-util)

(require 'rudel-infinote-states)

(require 'rudel-infinote-group) ;; TODO temp?
(require 'rudel-infinote-group-directory)
(require 'rudel-infinote-group-document)

(require 'rudel-infinote-text-document) ;; TODO temp

(require 'adopted)


;;; Class rudel-infinote-client-connection
;;

;; TODO make this a base class for client and server
(defclass rudel-infinote-client-connection (;;rudel-state-machine
					    rudel-connection)
  ((transport       :initarg  :transport
		    :type     rudel-transport
		    :documentation
		    "")
   (groups          :initarg  :groups
		    :type     hash-table
		    :documentation
		    "")
   (nodes           :initarg  :nodes
		    :type     list
		    :initform nil
		    :documentation
		    "")
   (sequence-number :initarg  :sequence-number ;; TODO this belongs in the group class?
		    :type     (integer 0)
		    :initform 0
		    :documentation
		    ""))
  "TODO")

(defmethod initialize-instance ((this rudel-infinote-client-connection)
				slots)
  ""
  ;; Initialize slots of THIS.
  (when (next-method-p)
    (call-next-method))

  ;; Create hash-table for groups.
  (with-slots (groups) this
    (setq groups (make-hash-table :test #'equal)))

  ;; Install handler.
  (with-slots (transport) this
    (rudel-transport-set-handler transport
				 (lexical-let ((this1 this))
				   (lambda (xml)
				     (rudel-receive this1 xml)))))

  ;;
  (with-slots (session) this
    (let ((user (rudel-user "scymtym" :color "red"))) ;(plist-get info ;; TODO

      (with-slots (self) session
	(setq self user))) ;; TODO temp

    ;; The special 'InfDirectory' group is there from the beginning.
    (let ((group (rudel-infinote-group-directory
		  "InfDirectory"
		  :publisher "you"))) ;; TODO use correct publisher name
      (rudel-add-group this group)

      (require 'rudel-infinote-directory-document)
      (rudel-add-document session
			  (rudel-infinote-directory-document
			   "root"
			   :id     0
			   :parent nil
			   :group  group)))) ;; TODO temp


  ;; ;; Register states.
  ;; (rudel-register-states this rudel-infinote-client-states)
  )

;; (defmethod rudel-register-state ((this rudel-infinote-client-connection)
;; 				 symbol state)
;;   ""
;;   ;; Associate THIS connection to STATE.
;;   (oset state :connection this)
;;
;;   ;;
;;   (call-next-method)
;;   )

(defmethod rudel-get-group ((this rudel-infinote-client-connection) name)
  "Return group named NAME or nil if there is no such group."
  (with-slots (groups) this
    (gethash name groups)))

(defmethod rudel-add-group ((this rudel-infinote-client-connection) group)
  ""
  (with-slots ((name :object-name) connection) group
    ;;
    (setq connection this) ;; TODO encapsulation violation?

    ;;
    (with-slots (groups) this
      (puthash name group groups)))
  )

(defmethod rudel-remove-group ((this rudel-infinote-client-connection)
			       name)
  ""
  (with-slots (groups) this
    (remhash name groups)))

(defmethod rudel-add-node ((this rudel-infinote-client-connection) node)
  ""
  (object-add-to-list this :nodes node))

;; TODO implement
;; (defmethod rudel-remove-node ((this rudel-infinote-client-connection) node)
;;   ""
;;   )

(defmethod rudel-send ((this rudel-infinote-client-connection) xml)
  ""
  (with-slots (transport) this
    (rudel-send transport xml)))

(defmethod rudel-receive ((this rudel-infinote-client-connection) xml)
  ""
  (let ((name (xml-tag-name xml)))
    (cond
     ;;
     ((string= name "group")
      (let* ((name  (xml-tag-attr xml "name"))
	     (xml   (xml-tag-children xml))
	     (group (rudel-get-group this name))) ;; TODO handle group not found
	(if group
	    (rudel-accept group (car xml))
	  (display-warning
	   '(rudel infinote)
	   (format "Group not found: %s" "name")
	   :warning))) ;; TODO pass list or single element?
      ;; Our own state does not change
      nil)

     ;;
     (t
      (call-next-method)))) ;; TODO what is actually called here?
  )

(defmethod rudel-disconnect ((this rudel-infinote-client-connection)) ;; TODO maybe we could automatically delegate to the transport
  ""
  (with-slots (transport) this
    (rudel-disconnect transport)))

(defmethod rudel-publish ((this rudel-infinote-client-connection) document)
  ""
  ;; Create a new adopted context for DOCUMENT.
  ;(rudel-add-context this document)

  ;;<add-node
  ;;  parent="node_id"
  ;;  type="Type"
  ;;  name="Name"
  ;;  seq="seq_id">
  ;;    <sync-in />
  ;;    <subscribe />
  ;;</add-node>

  ;; Announce the new document to the server.
  (let ((group  (rudel-get-group this "InfDirectory"))
	(parent 0)
	(type   "InfText")
	(name   (object-name-string document)))
    (rudel-send group
		`(("add-node"
		   ("parent" . ,(format "%d" parent))
		   ("type"   . ,type)
		   ("name"   . ,name)))))
  ;; TODO should be a method of the directory group
  )

(defmethod rudel-subscribe-to ((this rudel-infinote-client-connection)
			       document)
  ""
  ;; Create a new adopted context for DOCUMENT.
  ;; TODO (rudel-add-context this document)

  ;; Subscribe to DOCUMENT's group in the directory group and then
  ;; join the session group associated to DOCUMENT.
  (lexical-let ((stage)
		(reporter (make-progress-reporter "Subscribing " 0.0 nil)))
    (flet ((display-progress (state)
	      (progress-reporter-force-update
	       reporter 0.5
	       (format "%s (%s) " stage (car state)))))

      ;; Announce the subscription to the server.
      (setq stage "Subscribing")
      (let ((group (rudel-get-group this "InfDirectory")))
	(with-slots (id) document
	  (rudel-switch group 'subscribing id))
	(rudel-state-wait group '(idle) nil #'display-progress))
      ;; TODO responsibility of the group?

      ;; Join the group of the document.
      (setq stage "Joining")
      (with-slots (group) document
	(rudel-switch group 'joining)
	(rudel-state-wait group '(idle) nil #'display-progress))
      ;; TODO responsibility of the document?
      ))

  ;; We receive a notification of our own subscription from the
  ;; server. TODO Or, do we? Consequently we do not add SELF to the
  ;; list of subscribed users of DOCUMENT.
  )

(defmethod rudel-unsubscribe-from ((this rudel-infinote-client-connection)
				   document)
  ""
  ;; Delete the jupiter context for DOCUMENT.
  ;; TODO (rudel-remove-context this document)

  ;; Announce the end of our subscription to the server.
  (with-slots (id group) document
    (rudel-send group
		`(("session-unsubscribe"
		   ("id" . ,(format "%d" id))))))
  ;; TODO the group should handle this
  ;; TODO maybe there should be a separate state for this?

  ;; We receive a notification of the end of our own subscription from
  ;; the server. TODO do we? Consequently we do not remove SELF from
  ;; the list of subscribed users of DOCUMENT.
  )

(defmethod rudel-add-document ((this rudel-infinote-client-connection)
			       id parent-id name type)
  (with-slots (session) this
    (let ((parent (and parent-id
		       (rudel-find-document session parent-id
					    #'eq #'rudel-id))))
      (unless (or (null parent-id) parent)
	;(signal
	(error "could not find parent node %d" parent-id))

      ;; TODO the backend should construct the appropriate document
      ;; object based on TYPE
      ;(let ((document
      (destructuring-bind (node . is-document)
	  (cond
	   ;;
	   ((string= type "InfText")
	    (cons (rudel-infinote-text-document
		   name
		   :id     id
		   :parent parent)
		  t))

	   ;;
	   ((string= type "InfSubdirectory")
	    (cons (rudel-infinote-directory-document
		   name
		   :id     id
		   :parent parent
		   :group  (rudel-get-group this "InfDirectory"))
		  nil)))

	;;
	(rudel-add-node  session node)
	(rudel-add-child parent  node)
	(when is-document
	  (rudel-add-document session node)))))
  )

;; TODO rudel-remove-document

(defmethod rudel-subscribe-session ((this rudel-infinote-client-connection)
				    name method id)
  ""
  ;; TODO this makes sense for document sessions only, but we want to
  ;; subscribe to directories, too
  (with-slots (session) this
    (require 'rudel-infinote-group-text-document) ;; TODO temp
    (let* ((document (rudel-find-document session id
					  #'eq #'rudel-id))
	   (group    (rudel-infinote-group-text-document ;; TODO class
		      name
		      :publisher "you" ;; TODO temp
		      :method    method
		      ;;:id        id
		      :document  document)))

      (rudel-add-group this group)
      (oset document :group group))) ;; TODO temp
  )

(defmethod rudel-local-insert ((this rudel-infinote-client-connection)
			       document position data)
  ""
  (rudel-local-operation
   this
   document
   (adopted-insert "insert"
		   :from position
		   :data data))
  )

(defmethod rudel-local-delete ((this rudel-infinote-client-connection)
			       document position length)
  ""
  (rudel-local-operation
   this
   document
   (adopted-delete "delete"
		   :from position
		   :to   (+ position length)))
  )

(defmethod rudel-local-operation ((this rudel-infinote-client-connection)
				  document operation)
  "Handle OPERATION performed on DOCUMENT by sending a message through THIS connection."
  ;; Find jupiter context for DOCUMENT.
  ;;(let ((context (rudel-find-context this document)))

  ;; Notify the server of the operation
  (with-slots (id group) document
    (rudel-send group
		(rudel-infinote-embed-in-request (rudel-infinote-user "jan" :user-id 2) ;; TODO user
		  (rudel-operation->xml operation))))

  ;; Submit the operation to the jupiter context.
  ;; (jupiter-local-operation context operation))
  )

(defmethod rudel-remote-operation ((this rudel-infinote-client-connection)
				   document user
				   remote-revision local-revision
				   operation)
  "Handle OPERATION received through THIS connection performed by USER on DOCUMENT."
  (let* (;; Find jupiter context for DOCUMENT.
	 ;;(context     (rudel-find-context this document))
	 ;; And transform the operation.
	 (transformed operation)) ;;(jupiter-remote-operation
		       ;;context
		       ;;remote-revision local-revision
		       ;;operation)))

    ;; Apply the transformed operation to the document.
    (rudel-remote-operation document user transformed))
  )

(provide 'rudel-infinote-client)


;;; Unit tests
;;

;; (when nil
;;
;;   (rudel-send connection
;;   	      '(("message"
;;   		 ("from"     . "juliet@example.com")
;;   		 ("to"       . "romeo@example.net")
;;                  ("xml:lang" . "en"))
;;   		("body"
;;   		 "Art thou not Romeo, and a Montague?")))
;;
;;   (rudel-close connection))

;;; rudel-infinote-client.el ends here
