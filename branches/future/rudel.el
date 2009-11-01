;;; rudel.el --- A collaborative editing framework for Emacs
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Version: 0.2
;; Keywords: rudel, collaboration
;; URL: http://rudel.sourceforge.net/
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
;; Rudel is a framework for collaborative editing in Emacs.  Its
;; architecture allows communication with arbitrary collaborative
;; editors.


;;; History:
;;
;; 0.1 - Initial revision.


;;; Code:
;;

(eval-when-compile
  (require 'cl))

(require 'eieio)
(require 'eieio-base)

(require 'eieio-speedbar) ;; TODO required for now

(require 'rudel-util)
(require 'rudel-backend)
(require 'rudel-session-initiation)
(require 'rudel-operations)
(require 'rudel-operators)
(require 'rudel-overlay)
(require 'rudel-hooks)
(require 'rudel-interactive) ;; for `rudel-read-backend',
			     ;; `rudel-read-document',
			     ;; `rudel-read-session'
(require 'rudel-icons)
(require 'rudel-compat) ;; for `read-color' replacement


;;; Global variables
;;

(defconst rudel-version '(0 1)
  "Version of the Rudel framework.")

(defvar rudel-current-session nil
  "Global object representing the current Rudel session.
nil if there is no active session.")

(defvar rudel-buffer-document nil
  "Buffer-local variable which holds the Rudel document associated with the buffer.")
(make-variable-buffer-local 'rudel-buffer-document)
(put 'rudel-buffer-document 'permanent-local t)

(defvar rudel-buffer-change-workaround-data nil
  "Buffer-local variable which holds change data that could not be accessed otherwise.
It would be nice to find another way to do this.")
(make-variable-buffer-local 'rudel-buffer-change-workaround-data)
(put 'rudel-buffer-change-workaround-data 'permanent-local t)


;;; Customization
;;

(defgroup rudel nil
  "Rudel collaborative editing framework."
  :group 'applications
  :link  '(url-link :tag "Report Issues"
		    "http://sourceforge.net/tracker/?group_id=249139")
  :link  '(url-link :tag "Ask Questions (Mailing List)"
		    "http://sourceforge.net/mail/?group_id=249139")
  :link  '(url-link :tag "Ask Questions (EmacsWiki)"
		    "http://www.emacswiki.org/emacs/Rudel"))

(defcustom rudel-allocate-buffer-function
  'rudel-allocate-buffer-clear-existing
  "A function used to find or create buffers to associate to documents.
The function is called with the document name as the sole
argument and has to return a buffer object which will be attached
to the document in question."
  :group   'rudel
  :type    '(choice (const :tag "Clear content of existing buffer"
			   rudel-allocate-buffer-clear-existing )
		    (const :tag "Create a new uniquely named buffer"
			   rudel-allocate-buffer-make-unique )
		    (function :tag "Other function"))
  :require 'rudel-interactive)

(defcustom rudel-default-username (user-login-name)
  "*"
  :group 'rudel
  :type  '(string))


;;; Class rudel-session
;;

(defclass rudel-session (rudel-hook-object)
  ((backend              :initarg  :backend
			 :type     rudel-backend-child
			 :documentation
			 "The backend used by this session.")
   (users                :initarg  :users
			 :type     list
			 :initform nil
			 :documentation
			 "The list of users participating in this
session.")
   (documents            :initarg  :documents
			 :type     list
			 :initform nil
			 :documentation
			 "This list of documents available in
this session.")
   ;; Hooks
   (end-hook             :initarg  :end-hook
			 :type     list
			 :initform nil
			 :documentation
			 "")
   (add-user-hook        :initarg  :add-user-hook
			 :type     list
			 :initform nil
			 :documentation
			 "This hook is run when a user gets added
to the session.
The arguments are the session and the user object.")
   (remove-user-hook     :initarg  :remove-user-hook
			 :type     list
			 :initform nil
			 :documentation
			 "This hook is run when a user gets
removed from the session.
The arguments are the session and the user object.")
   (add-document-hook    :initarg  :add-document-hook
			 :type     list
			 :initform nil
			 :documentation
			 "This hook is run when a document gets
added to the session.
The arguments are the session and the document object.")
   (remove-document-hook :initarg  :remove-document-hook
			 :type     list
			 :initform nil
			 :documentation
			 "This hook is run when a document gets
removed from the session.
The arguments are the session and the document object."))
  "This class serves as a base class for rudel-client-session and
rudel-server-session. Consequently, it consists of slots common
to client and server sessions."
  :abstract t)

(defmethod rudel-end ((this rudel-session))
  "Terminate THIS session performing all necessary cleanup."
  ;; Run the hook.
  (object-run-hook-with-args this 'end-hook))

(defmethod rudel-add-user ((this rudel-session) user)
  "Add USER to the user list of THIS session.

Runs object hook (see `rudel-hook-object') `add-user-hook' with
arguments THIS and USER."
  ;; Add USER to list.
  (object-add-to-list this :users user)

  ;; Run the hook.
  (object-run-hook-with-args this 'add-user-hook user))

(defmethod rudel-remove-user ((this rudel-session) user)
  "Remove USER from the user list of THIS session.

Runs object hook (see `rudel-hook-object') `remove-user-hook'
with arguments THIS and USER."
  ;; Remove USER from list.
  (object-remove-from-list this :users user)

  ;; Run the hook.
  (object-run-hook-with-args this 'remove-user-hook user))

(defmethod rudel-find-user ((this rudel-session)
			    which &optional test key)
  "Find user WHICH in the user list.
WHICH is compared to the result of KEY using TEST."
  (unless test
    (setq test #'string=))
  (unless key
    (setq key #'object-name-string))
  (with-slots (users) this
    (find which users :key key :test test))
  )

(defmethod rudel-add-document ((this rudel-session) document)
  ""
  (unless (slot-boundp document :session)
    (oset document :session this))

  ;; Add DOCUMENT to the list of documents in THIS session.
  (object-add-to-list this :documents document)

  ;; Run the hook.
  (object-run-hook-with-args this 'add-document-hook document))

(defmethod rudel-remove-document ((this rudel-session) document)
  "Remove DOCUMENT from THIS session, detaching it if necessary."
  ;; Detach document from its buffer when necessary.
  (when (rudel-attached-p document)
    (rudel-detach-from-buffer document))

  ;; Remove DOCUMENT from the list of documents in THIS session.
  (object-remove-from-list this :documents document)

  ;; Run the hook.
  (object-run-hook-with-args this 'remove-document-hook document))

(defmethod rudel-find-document ((this rudel-session)
				which &optional test key)
  "Find document WHICH in the document list.
WHICH is compared to the result of KEY using TEST."
  (unless test
    (setq test #'string=))
  (unless key
    (setq key #'object-name-string))
  (with-slots (documents) this
    (find which documents :key key :test test))
  )

(defmethod rudel-unsubscribed-documents ((this rudel-session))
  ""
  (unless (slot-boundp this :self)
    (error "Cannot find unsubscribed documents unless slot self is bound"))
  (with-slots (documents self) this
    (remove-if
     (lambda (document)
       (with-slots (subscribed) document
	 (memq self subscribed)))
       documents))
  )


;;; Class rudel-client-session
;;
(defclass rudel-client-session (rudel-session)
  ((connection :initarg  :connection
	       :type     (or null rudel-connection-child)
	       :initform nil
	       :documentation
	       "The connection used for communication by this
session.")
   (self       :initarg  :self
	       :type     rudel-user-child
	       :documentation
	       "Points into USERS to the user object representing
the local user"))
  "Objects represent a collaborative editing session from a
client perspective.")

(defmethod rudel-end ((this rudel-client-session))
  ;; Clean everything up
  (with-slots (connection users documents) this
    ;; Detach all documents from their buffers
    (mapc #'rudel-detach-from-buffer documents)

    ;; Terminate the connection
    (when connection
      (condition-case nil
	  (rudel-disconnect connection)
	(error nil))))

  ;;
  (when (next-method-p)
    (call-next-method))
  )


;;; Class rudel-server-session
;;

(defclass rudel-server-session (rudel-session)
  ()
  "Class rudel-server-session "
  :abstract t)


;;; Class rudel-connection
;;

(defclass rudel-connection ()
  ((session :initarg :session
	    :type    rudel-session-child
	    :documentation
	    ""))
  "This abstract class defines the interface implementations of
client protocols have to obey."
  :abstract t)

(defgeneric rudel-disconnect ((this rudel-connection))
  "Close the connection.")

(defgeneric rudel-change-color- ((this rudel-connection) color) ;; TODO name
  "")

(defgeneric rudel-publish ((this rudel-connection) document)
  "")

(defgeneric rudel-subscribe-to ((this rudel-connection) document) ;; TODO name should be rudel-subscribe
  "")

(defgeneric rudel-unsubscribe-from ((this rudel-connection) document) ; TODO name should be rudel-unsubscribe
  "")

(defgeneric rudel-local-insert ((this rudel-connection))
  "")

(defgeneric rudel-local-delete ((this rudel-connection))
  "")

(defgeneric rudel-remote-insert ((this rudel-connection))
  "")

(defgeneric rudel-remote-delete ((this rudel-connection))
  "")


;;; Buffer-related functions
;;

;;;###autoload
(defun rudel-buffer-has-document-p (&optional buffer)
  "Return non-nil if a document object is attached to BUFFER.
If BUFFER is nil, use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))

  (with-current-buffer buffer
    rudel-buffer-document))

(defun rudel-buffer-document (&optional buffer)
  "Return the document object attached to BUFFER.
If BUFFER is nil, use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))

  (with-current-buffer buffer
    rudel-buffer-document))

(defun rudel-set-buffer-document (document &optional buffer)
  "TODO"
  (unless buffer
    (setq buffer (current-buffer)))

  (with-current-buffer buffer
    (setq rudel-buffer-document document)))

(defun rudel-buffer-has-document-p (&optional buffer)
  "Return non-nil if a document object is attached to BUFFER.
If BUFFER is nil, use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))

  (buffer-local-value 'rudel-buffer-document buffer))

(defun rudel-buffer-document (&optional buffer)
  "Return the document object attached to BUFFER.
If BUFFER is nil, use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))

  (buffer-local-value 'rudel-buffer-document buffer))

(defun rudel-set-buffer-document (document &optional buffer)
  "Associate BUFFER to DOCUMENT.
If DOCUMENT is nil, make it not associated to any buffer.
If BUFFER is nil, use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))

  (with-current-buffer buffer
    (setq rudel-buffer-document document)))

(defun rudel-handle-buffer-change (from to length)
  "Handle buffer change at range FROM to TO with length LENGTH by relaying them to the document object of the buffer.
See after-change-functions for more information."
  (when (rudel-buffer-has-document-p)
    (let ((document (rudel-buffer-document))
	  (text)) ; TODO with-rudel-buffer-document?
      (cond
       ;; The change was an insert
       ((and (/= from to)
	     (zerop length))
	(with-slots (buffer) document
	  (with-current-buffer buffer
	    (setq text (buffer-substring-no-properties from to)))
	  (rudel-local-operation document
				 (rudel-insert-op
				  "insert"
				  :from (- from 1)
				  :data text))))

       ;; The change was a delete
       ((and (= from to)
	     (not (zerop length)))
	(rudel-local-operation document
			       (rudel-delete-op
				"delete"
				:from   (- from 1)
				:length length)))

       ;; The operation was neither an insert nor a delete. This seems
       ;; to mean that the region has changed arbitrarily. The only
       ;; option we have is sending a delete and corresponding insert
       ;; message that emulate the change.
       (t
	(with-slots (buffer) document
	  (with-current-buffer buffer
	    (setq text (buffer-substring-no-properties from to)))
	  (rudel-local-operation document
				 (rudel-delete-op
				  "delete"
				  :from   (- from 1)
				  :length length))
	  (rudel-local-operation document
				 (rudel-insert-op
				  "insert"
				  :from (- from 1)
				  :data text)))))))
  )

(defun rudel-buffer-change-workaround (from to)
  (when (/= from to)
    (setq rudel-buffer-change-workaround-data
	  (list from to
		(buffer-substring-no-properties from to)))))


;;; Protection against major mode changes
;;

(defvar rudel-mode-changed-buffers nil
  "List of buffers that may need to be repaired after a major
  mode change.")

(defun rudel-handle-major-mode-change ()
  "Store the current buffer to repair damage done by major mode change.

Note: The way this works is inspired by mode-local.el by David
Ponce and Eric M. Ludlam."
  ;; Store the buffer for later repair.
  (add-to-list 'rudel-mode-changed-buffers (current-buffer))

  ;; Schedule `rudel-after-major-mode-change' to run after the
  ;; command, that caused the major mode change.
  (add-hook 'post-command-hook
	    #'rudel-after-major-mode-change)
  )

(defun rudel-after-major-mode-change ()
  "Repair damage done by major mode changes.
As a function in `post-command-hook', this is run after there was
a `major-mode' change.

Note: The way this works is inspired by mode-local.el by David
Ponce and Eric M. Ludlam."
  ;; Remove this function from `post-command-hook'.
  (remove-hook 'post-command-hook
	       #'rudel-after-major-mode-change)

  ;; Repair all buffers affected by the major mode change.
  (dolist (buffer rudel-mode-changed-buffers)
    (let ((document (buffer-local-value 'rudel-buffer-document
					buffer)))
      (rudel-attach-to-buffer document buffer)))
  )


;;; Interactive functions
;;

;;;###autoload
(defun rudel-join-session (info)
  "Join the collaborative editing session described by INFO.
INFO is a property list that describes the collaborative editing
session in terms of properties like :host, :port
and :encryption. The particular properties and their respective
meanings depend on the used backend.

When called interactively, all data required to join a session
will be prompted for."
  (interactive
   ;; Try the discover method of session initiation backends to find
   ;; available sessions.
   (list
    (let ((info)
	  (session-initiation-backend))
      ;; Query the chosen backend until session info is available.
      (while (not info)
	(message "Discovering Sessions ...")
	(let* ((sessions   (rudel-session-initiation-discover
			    session-initiation-backend))
	       (maybe-info (if (= (length sessions) 1)
			       (car sessions)
			     (rudel-read-session
			      sessions "Choose Session: " 'object))))
	  (if (rudel-backend-cons-p maybe-info)
	      (setq session-initiation-backend (car maybe-info))
	    (setq info maybe-info))))
      info)))

  ;; First, create the session object.
  (let* ((session-name      (plist-get info :name))
	 (transport-backend (cdr (plist-get info :transport-backend)))
	 (protocol-backend  (cdr (plist-get info :protocol-backend)))

	 ;; First, create the session object.
	 (session           (rudel-client-session
			     session-name
			     :backend protocol-backend))
	 (transport)
	 (connection))

    ;; Store session in INFO.
    (setq info (plist-put info :session session))

    ;; Create transport object
    (condition-case error-data
	(flet ((augment-info (backend info)
	         (append
		  (list
		   (intern (concat ":" (read-string "Key: ")))
		   (read-string "Value: "))
		  info)))
	  (setq transport (rudel-make-connection
			   transport-backend
			   info #'augment-info)))
      (error (error "Could not connect using transport backend `%s' with %s: %s"
		    (object-name-string transport-backend)
		    info
		    (car error-data))))

    ;; Create connection
    (condition-case error-data
	(lexical-let ((reporter (make-progress-reporter "Joining ")))
	  (flet ((display-progress (state) ;; TODO Think about this callback interface
		   (cond
		    ;; For all states, just spin.
		    ((consp state)
		     (progress-reporter-force-update
		      reporter nil (format "Joining (%s)" (car state))))

		    ;; Done
		    (t
		     (progress-reporter-force-update
		      reporter nil "Joining ")
		     (progress-reporter-done reporter)))))
	    (setq connection
		  (rudel-connect protocol-backend
				 transport
				 info #'display-progress))))
      (error (error "Could not connect using protocol backend `%s' with %s: %s"
		    (object-name-string protocol-backend)
		    info
		    (car error-data))))
    (oset session :connection connection)

    ;; Store the new session object globally.
    (setq rudel-current-session session)
    ;;(add-to-list 'rudel-sessions session)) ; only one session for now

    ;; Reset the global session variable when the session ends.
    (object-add-hook session 'end-hook
		     (lambda (session)
		       (setq rudel-current-session nil)))

    ;; Run the hook.
    (run-hook-with-args 'rudel-session-start-hook session)

    session))

;;;###autoload
(defun rudel-host-session ()
  "Host a collaborative editing session.
All data required to host a session will be prompted for
interactively."
  (interactive)
  ;; If necessary, ask the user for the backend we should use.
  (let* ((backend (cdr (rudel-backend-choose
			'protocol
			(lambda (backend)
			  (rudel-capable-of-p backend 'host)))))
	 (info    (rudel-ask-host-info backend))
	 (server))

    ;; Try to create the server
    (condition-case error-data
	(setq server (rudel-host backend info))
      ('error
       (error "Could not host session using backend `%s' with %s: %s"
	      (object-name-string backend)
	      info
	      (car error-data))))
    server))

;;;###autoload
(defun rudel-end-session ()
  "End the current collaborative editing session."
  (interactive)
  (unless rudel-current-session
    (error "No active Rudel session"))

  ;; Actually end the session.
  (rudel-end rudel-current-session)
  )

;;;###autoload
(defun rudel-change-color ()
  "Change the color associated with the local user.
Not all backends support this operation."
  (interactive)
  ;; Make sure we have a session.
  (unless rudel-current-session
    (error "No active Rudel session"))

  (with-slots (backend connection self) rudel-current-session
    ;; Make sure the backend can change colors.
    (unless (rudel-capable-of-p backend 'change-color)
      (error "Backend `%s' cannot change colors"
	     (object-name-string backend)))

    (with-slots ((name :object-name) color) self
      ;; Ask the user for a new color.
      (setq color (read-color "New Color: " t))

      ;; Tell the connection to announce the change and change it in
      ;; our user object.
      (rudel-change-color- connection color)

      ;; Run the change hook.
      (object-run-hook-with-args self 'change-hook)

      ;; Update overlay color.
      (rudel-overlay-set-face-attributes
       (rudel-overlay-make-face-symbol 'author name)
       color)))
  )

;;;###autoload
(defun rudel-subscribe (document)
  "Subscribe to DOCUMENT offered by a peer in a collaborative editing session.
When called interactively, DOCUMENT is prompted for interactively."
  (interactive
   (list (progn
	   ;; We have to retrieve the document list from an active
	   ;; session.
	   (unless rudel-current-session
	     (error "No active Rudel session"))
	   ;; Select unsubscribed documents.
	   (let ((documents (rudel-unsubscribed-documents
			     rudel-current-session)))
	     ;; Already subscribed to all documents. This is an error.
	     (when (null documents)
	       (error "No unsubscribed documents"))
	     ;; Read an unsubscribed document.
	     (rudel-read-document documents nil 'object)))))

  ;; Make sure we have a session.
  (unless rudel-current-session
    (error "No active Rudel session"))

  ;; Create a new buffer and attach the document to it.
  (let* ((name   (rudel-suggested-buffer-name document))
	 (buffer (funcall rudel-allocate-buffer-function name)))
    (rudel-attach-to-buffer document buffer)

    (let ((connection (oref (oref document :session) :connection)))
      (rudel-subscribe-to connection document))

    ;; Show the new buffer.
    (set-window-buffer nil buffer))
  )

;;;###autoload
(defun rudel-publish-buffer (&optional buffer)
  "Make the BUFFER available for subscription to peers in a collaborative editing session.
If BUFFER is nil, the current buffer is used."
  (interactive (list nil))

  ;; Make sure we have a session.
  (unless rudel-current-session
    (error "No active Rudel session"))

  (unless buffer
    (setq buffer (current-buffer)))

  (with-current-buffer buffer
    (when (rudel-buffer-has-document-p)
      (error "Buffer already published or subscribed"))) ; TODO keep this?

  ;;
  (with-slots (backend connection self) rudel-current-session
    (let ((document (rudel-make-document backend
					 (buffer-name buffer)
					 rudel-current-session)))
      (rudel-add-document rudel-current-session document)

      (rudel-attach-to-buffer document buffer)
      (object-add-to-list document :subscribed self)

      (rudel-publish connection document)))
  )

;;;###autoload
(defun rudel-unpublish-buffer (&optional buffer)
  "Deny peers access to BUFFER in a collaborative editing session.
If BUFFER is nil, the current is used."
  (interactive)

  ;; Make sure we have a session.
  (unless rudel-current-session
    (error "No active Rudel session"))

  (unless buffer
    (setq buffer (current-buffer)))

  (with-current-buffer buffer
    (unless (rudel-buffer-has-document-p)
      (error "Buffer is not published")))

  ;;
  (with-slots (connection) rudel-current-session
    (let ((document (rudel-buffer-document buffer)))
      (rudel-detach-from-buffer document)

      (rudel-unsubscribe-from connection document)))
  )

(provide 'rudel)
;;; rudel.el ends here
