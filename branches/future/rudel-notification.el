;;; rudel-notification.el --- Notification facilities for Rudel
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, notification, libnotify
;; X-RCS: $Id:$
;;
;; This file is part of Rudel.
;;
;; Rudel is free software; you can redistribute it and/or modify it
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
;; along with this program. If not, see <http://www.gnu.org/licenses>.


;;; Commentary:
;;
;; This file contains the function `rudel-notify' which dispatches
;; notification events to configured handler functions.
;;
;; Certain objects like Rudel documents and users are automatically
;; converted to suitable textual or graphical representations for the
;; notification.
;;
;; Example:
;;
;; (rudel-notify
;;  "New document %s"
;;  "A new document called %s was added by user %s"
;;  (rudel-document "Doc")
;;  (rudel-user "John"))


;;; History
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'rudel-user)
(require 'rudel-document)

(require 'rudel-display)


;;; Customization
;;

(defcustom rudel-notify-function
  #'rudel-notify-emacs-message
  "Function used to convey Rudel notification to the user."
  :group 'rudel
  :type  `(choice (const :tag "Do not display messages."
			 ignore)
		  (const :tag "Display a message Emacs's echo area."
			 rudel-notify-emacs-message)
		  ,@(when (or (featurep 'todochiku)
			      (locate-library "todochiku"))
		      '((const :tag "TODO"
			      rudel-notify-todochiku)))
		  ,@(when (or (featurep 'libnotify)
			      (locate-library "libnotify"))
		      '((const :tag "Display a notification using freedesktop's libnotify."
			       rudel-notify-libnotify)))
		  (function :tag "Other function"))
  )


;;; Interface
;;

;; TODO Something like this would work with libnotify:
;; '(:default (lambda () (message "default")))
;; '(:subscribe "subscribe" (lambda () (message "subscribe"))

(defun rudel-notify (summary text &rest args) ;; TODO importance argument? action arguments?
  "Do a notification with SUMMARY and detailed text TEXT with replacements ARGS.
TEXT has to have a number of format specifications equal the
number of args in ARGS."
  (funcall rudel-notify-function summary text args))


;;; Notification backend functions
;;

(defun rudel-notify-emacs-message (summary text args)
  ""
  (apply #'message
	 text
	 (mapcar #'rudel-notify-format-object args)))

(defun rudel-notify-libnotify (summary text args)
  ""
  (require 'libnotify)
  (let* ((formatted (mapcar #'rudel-notify-format-object args))
	 (summary   (apply #'format summary formatted))
	 (text      (apply #'format text
			   (mapcar
			    (lambda (text)
			      (format "<b>%s</b>" text))
			    formatted))))
    (libnotify-notify summary text "info" "Rudel"))
  )


;;; Helper functions
;;

(defun rudel-notify-format-object (object)
  ""
  (cond
   ((or (rudel-user-child-p object)
	(rudel-document-child-p object))
    (rudel-display-string object))

   (t object)))

(provide 'rudel-notification)
;;; rudel-notification.el ends here
