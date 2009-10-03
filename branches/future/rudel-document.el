;;; rudel-document.el --- Rudel document class
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, domain model, document
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
;; 0.2 - Hooks
;;
;; 0.1 - initial version


;;; Code:
;;

(require 'eieio)


;;; Class rudel-document
;;

(defclass rudel-document (eieio-named
			  rudel-hook-object) ;; TODO require providing library
  ((capabilities     :initarg :capabilities
		     :type    list
		     :documentation
		     "")
   (session          :initarg  :session ;; TODO when do we need this?
		     :type     rudel-session
		     :documentation
		     "")
   (buffer           :initarg  :buffer
		     :type     (or null buffer)
		     :initform nil
		     :documentation
		     "The buffer associated to this document or
nil.")
   (subscribed       :initarg  :subscribed
		     :type     list ; TODO of rudel-user-child
		     :initform nil
		     :documentation
		     "")
   ;; Hooks
   (subscribe-hook   :initarg  :subscribe-hook
		     :type     list
		     :initform nil
		     :documentation
		     "This hook is run when a user subscribes to
this document object.")
   (unsubscribe-hook :initarg  :unsubscribe-hook
		     :type     list
		     :initform nil
		     :documentation
		     "This hook is run when a user unsubscribes
from this document object.")
   (attach-hook      :initarg  :attach-hook
		     :type     list
		     :initform nil
		     :documentation
		     "This hook is run when a buffer is attached
to this document object.")
   (detach-hook      :initarg  :detach-hook
		     :type     list
		     :initform nil
		     :documentation
		     "This hook is run when the attached buffer
is detached from this document object."))
  "This class represents a document, which participants of a
collaborative editing session can subscribe to."
  :abstract t)

(defmethod rudel-unique-name ((this rudel-document))
  "Returns a suggested name for the buffer attached to THIS document."
  (object-name-string this))

(defmethod rudel-suggested-buffer-name ((this rudel-document))
  "Returns a suggested name for the buffer attached to THIS document."
  (rudel-unique-name this))

(defmethod rudel-attach-to-buffer ((this rudel-document) buffer)
  "Attach THIS document to BUFFER"
  (with-slots ((doc-buffer :buffer)) this
    ;; Set buffer slot of THIS to BUFFER and associated THIS with
    ;; BUFFER.
    (setq doc-buffer buffer)
    (rudel-set-buffer-document this buffer)

    (with-current-buffer doc-buffer
      ;; Add the handler function for buffer changes to the buffer's
      ;; change hook.
      (add-hook 'after-change-functions
		#'rudel-handle-buffer-change
		nil t)

      ;; Store change data before the change a done. This is necessary
      ;; because the number of bytes (not characters) cannot otherwise
      ;; be recovered after a deletion.
      (add-hook 'before-change-functions
		#'rudel-buffer-change-workaround
		nil t)

      ;; Add a handler to the kill-buffer hook to unsubscribe from the
      ;; document when the buffer gets killed.
      (add-hook 'kill-buffer-hook
		#'rudel-unpublish-buffer
		nil t) ;; TODO should we warn the user?

      ;; TODO explain
      (add-hook 'change-major-mode-hook
		#'rudel-handle-major-mode-change
		nil t))

    ;; Run the hook.
    (object-run-hook-with-args this 'attach-hook doc-buffer))
  )

(defmethod rudel-detach-from-buffer ((this rudel-document))
  "Detach document THIS from its buffer.
Do nothing, if THIS is not attached to any buffer."
  (with-slots (buffer) this
    (let ((buffer-save buffer))

      ;; Only try to detach from BUFFER, if it is non-nil. BUFFER can
      ;; be nil, if the user did not subscribe to the document, or
      ;; unsubscribed after subscribing.
      (when buffer

	(with-current-buffer buffer
	  ;; Remove our handler function from the after-change hook.
	  (remove-hook 'after-change-functions
		       #'rudel-handle-buffer-change
		       t)

	  ;; Remove our handler function from the before-change hook.
	  (remove-hook 'before-change-functions
		       #'rudel-buffer-change-workaround
		       t)

	  ;; Remove our handler function from the kill-buffer hook.
	  (remove-hook 'kill-buffer-hook
		       #'rudel-unpublish-buffer
		       t)

	  ;; Remove all overlays.
	  (rudel-overlays-remove-all)

	  ;; Remove the major mode change handler.
	  (remove-hook 'change-major-mode-hook
		       #'rudel-handle-major-mode-change
		       t))

	;; Unset buffer slot of THIS and delete association of THIS
	;; with BUFFER.
	(rudel-set-buffer-document buffer nil)
	(setq buffer nil))

      ;; Run the hook.
      (object-run-hook-with-args this 'detach-hook buffer-save)))
  )

(defmethod rudel-add-user ((this rudel-document) user)
  "Add USER to the list of subscribed users of THIS.

Runs object hook (see `rudel-hook-object') `subscribe-hook' with
arguments THIS and USER."
  ;; Add USER to list.
  (object-add-to-list this :subscribed user)

  ;; Run the hook.
  (object-run-hook-with-args this 'subscribe-hook user))

(defmethod rudel-remove-user ((this rudel-document) user)
  "Remove USER from the list of subscribed users of THIS.

Runs object hook (see `rudel-hook-object') `unsubscribe-hook'
with arguments THIS and USER."
  ;; Remove USER from list.
  (object-remove-from-list document :subscribed user)

  ;; Run the hook.
  (object-run-hook-with-args this 'unsubscribe-hook user))

(defmethod rudel-insert ((this rudel-document) position data)
  "Insert DATA at POSITION into the buffer attached to THIS.
When POSITION is nil `point-max' is used to determine the
insertion position.
Modification hooks are disabled during the insertion."
  (with-slots (buffer) this
    (save-excursion
      (set-buffer buffer)

      (unless position
	(setq position (- (point-max) 1)))

      (let ((inhibit-modification-hooks t))
	(goto-char (+ position 1))
	(insert data))))
  )

(defmethod rudel-delete ((this rudel-document) position length)
  "Delete a region of LENGTH character at POSITION from the buffer attached to THIS.
Modification hooks are disabled during the insertion."
  (with-slots (buffer) this
    (save-excursion
      (set-buffer buffer)

      (let ((inhibit-modification-hooks t))
	(delete-region (+ position 1) (+ position length 1)))))
  )

(defmethod rudel-local-operation ((this rudel-document) operation)
  "Apply the local operation OPERATION to THIS."
  (with-slots (session buffer) this
    (with-slots (connection (user :self)) session
      (dolist (operations (list

			   ;; Update overlays
			   (rudel-overlay-operators
			    "overlay-operators"
			    :document this
			    :user     user)

			   ;; Notify connection TODO not document's responsibility
			   (rudel-connection-operators
			    "connection-operators"
			    :connection connection
			    :document   this)

			   ;; Run the appropriate hook TODO not document's responsibility
			   (rudel-hook-operators
			    "hook-operations"
			    :document this
			    :user     user)))

	;; Apply the operation using each set of operators
	(rudel-apply operation operations))))
  )

(defmethod rudel-remote-operation ((this rudel-document) user operation)
  "Apply the remote operation OPERATION performed by USER to THIS."
  (dolist (operations (append ;; TODO use list and name operators

		       ;; Update buffer contents
		       (list (rudel-document-operators
			      "document-operators"
			      :document this))

		       ;; Update overlays
		       (when user
			 (list (rudel-overlay-operators
				"overlay-operators"
				:document this
				:user     user)))

		       ;; Run the appropriate hook TODO not document's responsibility
		       (list (rudel-hook-operators
			      "hook-operations"
			      :document this
			      :user     user))))

    ;; Apply the operation using each set of operators
    (rudel-apply operation operations))
  )

(defmethod rudel-chunks ((this rudel-document))
  "Return a list of text chunks of the associated buffer.
Each element in the chunk is a list structured like this (START
END AUTHOR). START and END are numbers, AUTHOR is of type (or
null rudel-user-child)."
  (with-slots (buffer) this
    (unless buffer
      (error "Not attached to buffer")) ;; TODO

    ;; Extract buffer string and a list of chunks partitioning the
    ;; string according to the respective author (or nil).
    (with-current-buffer buffer
      (let ((string         (buffer-string)) ;; TODO no-properties?
	    (overlay-chunks (mapcar
			     (lambda (overlay)
			       (list (- (overlay-start overlay) 1)
				     (- (overlay-end   overlay) 1)
				     (rudel-overlay-user overlay)))
			     (sort* (rudel-author-overlays)
				    '< :key 'overlay-start)))
	    (last)
	    (augmented-chunks))

	;; Iterate through the list of chunks to find gaps between
	;; chunks (also before the first) and insert entries with
	;; author nil accordingly.
	(dolist (chunk overlay-chunks)
	  (when (or (and (not last)
			 (> (nth 0 chunk) 0))
		    (and last
			 (/= (nth 1 last)
			     (nth 0 chunk))))
	    (push (list (if last (nth 1 last) 0)
			(nth 0 chunk)
			nil)
		  augmented-chunks))
	  (push chunk augmented-chunks)
	  (setq last chunk))

	;; If there is text after the last chunk, create another one
	;; with author nil. If there were no chunks at all, this chunk
	;; can also cover the whole buffer string.
	(when (or (and (not last)
		       (/= (length string) 0))
		  (and last
		       (/= (nth 1 last) (length string))))
	  (push (list (if last (nth 1 last) 0)
		      (length string)
		      nil)
		augmented-chunks))

	;; Sort chunks according to the start position.
	(sort* augmented-chunks '< :key 'car))))
  )

(provide 'rudel-document)
;;; rudel-document.el ends here
