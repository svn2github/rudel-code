;;; rudel-integration-tests.el ---
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.


;;; Commentary:
;;


;;; History:
;;


;;; Code:
;;

(defclass emacs-process ()
  ((process :initarg :process
	    :type    process
	    :documentation
	    "")
   (result  :initarg :result
	    :type    string
	    :documentation
	    ""))
  "")

(defvar emacsen-by-name (make-hash-table)
  "")

(defvar emacsen-by-process (make-hash-table)
  "")

(defvar init-code
  (let ((location "/home/jan")) ;; (locate-library "rudel"
    `((add-to-list 'load-path ,(format "%s/rudel/"          location))
      (add-to-list 'load-path ,(format "%s/rudel/jupiter/"  location))
      (add-to-list 'load-path ,(format "%s/rudel/obby/"     location))
      (add-to-list 'load-path ,(format "%s/rudel/zeroconf/" location))
      (require 'rudel-integration-tests)
      (make-eval-receiver)))
  "")

(defun filter (process data)
  ""
  (let ((emacs (gethash process emacsen-by-process)))
    (with-slots (result) emacs
      (setq result (concat result data)))))

(defun make-emacs-process (name)
  ""
  (let* ((name-string (symbol-name name))
	 (process     (start-process
		       name-string
		       (generate-new-buffer name-string)
		       "emacs"
		       "--no-init-file"
		       "--eval"
		       (mapconcat
			#'pprint-to-string
			init-code
			"\n")))
	 (emacs       (emacs-process name-string
				     :process process
				     :result "")))
    (puthash name emacs emacsen-by-name)
    emacs))

(defun run-code-in-emacs-process (emacs code)
  ""
  (let ((emacs (gethash emacs emacsen-by-name)))
    (with-slots (process) emacs
      (process-send-string process (pprint-to-string code)))))

(defmacro in-emacs (emacs &rest forms)
  ""
  `(run-code-in-emacs-process emacs ,forms))


;;;
;;

(defun make-eval-receiver ()
  ""
  (make-network-process
   :name   "bla"
   :server t
   :family 'local
   :local  socket-name)
  )

(defun receive-eval (process data)
  ""
  (let ((code (read data)))
    (process-send-string process (pprint-to-string (eval code)))))


;;;
;;

(setq p (make-emacs-process 'bla))
(run-code-in-emacs-process 'bla "bla")

(provide 'rudel-integration-tests)
;;; rudel-integration-tests.el ends here
