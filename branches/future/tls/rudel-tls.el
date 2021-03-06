;;; rudel-tls.el --- Start TLS protocol.
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, TLS, encryption, starttls, gnutls
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
;; This contains a simple implementation of the so called Start TLS
;; protocol, which means enabling TLS encryption for an existing
;; network connection.


;;; History:
;;
;; 0.1 - Initial revision.


;;; Code:
;;

(require 'format-spec)

(require 'rudel)
(require 'rudel-util)
(require 'rudel-tcp)


;;; Customization
;;

(defcustom rudel-tls-client-program
  "gnutls-cli"
  "The gnutls client program to use for encrypted connections."
  :group 'rudel
  :type  'file)

(defcustom rudel-tls-client-arguments
  "--starttls --kx ANON_DH --port %p %h"
  "Arguments passed to the gnutls client program."
  :group 'rudel
  :type  'string)


;;; Constants
;;

(defconst rudel-tls-transport-version '(0 1)
  "Version of the TLS transport backend for Rudel.")


;;; TLS functions
;;

(defun rudel-tls-make-process (&rest args)
  "Make a network process with keyword arguments ARGS.
This function works similar to `make-network-process'. Supported
keyword arguments are :name (ignore), :host, :port, :filter
and :sentinel. The returned process object is suitable for
start-TLS. Once the enclosing protocol indicates TLS encryption
should start, `rudel-tls-start-tls' can be called to enabled TLS
for the network connection."
  (let* ((host      (plist-get args :host)) ;; TODO clumsy
	 (port      (plist-get args :service))
	 (filter    (plist-get args :filter))
	 (sentinel  (plist-get args :sentinel))
	 ;; Compile the command to start the TLS binary.
	 (arguments (format-spec rudel-tls-client-arguments
				 (format-spec-make
				  ?h host
				  ?p (number-to-string port))))
	 ;; Start the TLS program.
	 (process (apply #'start-process
			 (format "*tls-%s*" host) nil
			 rudel-tls-client-program
			 (split-string arguments " "))))

    ;; Store filter function and attach proxy filter to handle TLS
    ;; handshake.
    (when filter
      (rudel-set-process-object process filter :old-filter))
    (set-process-filter process #'rudel-tls-wait-init)

    ;; Attach sentinel function.
    (when sentinel
      (set-process-sentinel process sentinel))

    ;; Mark the process as supporting TLS encryption
    (rudel-set-process-object process t :supports-tls)

    process)
  )

(defun rudel-tls-start-tls (process)
  "Enable TLS encryption for the network connection PROCESS.
This only works if PROCESS has been created by
`rudel-tls-make-process'."
  ;; Save current filter function.
  (rudel-set-process-object
   process (process-filter process) :old-filter)
  ;; Install TLS handshake filter function and signal program to start
  ;; TLS handshake.
  (message "tls-start-tls: switching to \"handshake\" filter")
  (set-process-filter process #'rudel-tls-wait-handshake)
  (signal-process process 'sigalrm)
  )

(defun rudel-tls-wait-init (process data)
  "Is installed as process filter on PROCESS until gnutls is done printing messages."
  ;; Retrieve complete lines.
  (let ((buffer (rudel-process-object process :buffer)))
    (rudel-assemble-fragments data buffer #'rudel-assemble-lines)
    (rudel-set-process-object process buffer :buffer))

  (let ((client-data)
	(old-filter (rudel-process-object process :old-filter))
	(client-mode))

    ;; Assemble lines that were not generated by gnutls. It is very
    ;; brittle to wait for last line of gnutls output like, but it
    ;; cannot be helped.
    (rudel-loop-fragments data line
      (if client-mode
	  (setq client-data (concat client-data line "\n"))
	(when (string-match-p "- Simple Client Mode.*" line)
	  (setq client-mode t))))

    ;; When there are any lines not generated by gnutls,
    ;; initialization is over. Process the data and install the old
    ;; filter function.
    (when client-data
      (funcall old-filter process client-data))
    (when client-mode
      (message "tls-wait-init: switching back to old filter")
      (set-process-filter process old-filter)))
  )

(defun rudel-tls-wait-handshake (process data)
  "Is installed as process filter on PROCESS while gnutls is doing the TLS handshake."
  ;; Retrieve complete lines.
  (let ((buffer (rudel-process-object process :buffer)))
    (rudel-assemble-fragments data buffer #'rudel-assemble-lines)
    (rudel-set-process-object process buffer :buffer))

  (let ((client-data)
	(old-filter  (rudel-process-object process :old-filter))
	(client-mode))

    ;; Assemble lines that were not generated by gnutls. It is very
    ;; brittle to wait for last line of gnutls output like, but it
    ;; cannot be helped.
    (rudel-loop-fragments data line
      (if client-mode
	  (setq client-data (concat client-data line "\n"))
	(when (string-match-p "- Compression.*" line)
	  (setq client-mode t))))

    ;; When there are any lines not generated by gnutls, handshake is
    ;; over. Process the data and install `established' filter
    ;; function.
    (when client-data
      (funcall old-filter process client-data))
    (when client-mode
      (message "tls-wait-handshake: switching to \"established\" filter")
      (set-process-filter process #'rudel-tls-established)
      (rudel-set-process-object process t :encryption)))
  )

(defun rudel-tls-established (process data)
  "Is installed as process filter on PROCESS after gnutls has established TLS encryption."
  ;; Retrieve complete lines.
  (let ((buffer (rudel-process-object process :buffer)))
    (rudel-assemble-fragments data buffer #'rudel-assemble-lines)
    (rudel-set-process-object process buffer :buffer))

  (let ((client-data)
	(old-filter (rudel-process-object process :old-filter)))

    ;; Assemble lines that were not generated by gnutls.
    (rudel-loop-fragments data line
      (unless (string-match-p "- Peer has closed the GNUTLS connection" line)
	(setq client-data (concat client-data line "\n"))))

    ;; When there are any lines not generated by gnutls, pass those to
    ;; the old filter function.
    (when client-data
      (funcall old-filter process client-data)))
  )


;;; Class rudel-start-tls-transport
;;

(defclass rudel-start-tls-transport (rudel-socket-transport)
  ()
  "Objects of this class provide TCP transports with START TLS
capability.")

(defmethod rudel-enable-encryption ((this rudel-start-tls-transport))
  "Try to enable TLS encryption on THIS transport."
  (with-slots (socket) this
    (rudel-tls-start-tls socket)))


;;; Class rudel-start-tls-backend
;;

;;;###autoload
(defclass rudel-start-tls-backend (rudel-transport-backend)
  ((capabilities :initform (connect)))
  "STARTTLS transport backend.
The transport backend is a factory for transport objects that
support STARTTLS behavior.")

(defmethod initialize-instance ((this rudel-start-tls-backend) slots)
  "Initialize slots and set version of THIS."
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-tls-transport-version))

(defmethod rudel-make-connection ((this rudel-start-tls-backend)
				  info info-callback
				  &optional progress-callback)
  "Connect to a START-TLS server using the information in INFO.
INFO has to be a property list containing the keys :host
and :port."
  ;; Ensure that INFO contains all necessary information.
  (unless (every (lambda (keyword) (member keyword info))
		 '(:host :port))
    (setq info (funcall info-callback this info)))

  ;; Extract information from INFO and create the socket.
  (let* ((host      (plist-get info :host))
	 (port      (plist-get info :port))
	 ;; Create the network process
	 (socket    (rudel-tls-make-process
		     :name     (format "STARTTLS TCP to %s" host)
		     :host     host
		     :service  port
		     :stop     t)))
    (rudel-socket-transport
     (format "to %s" host)
     :socket socket)))


;;; Autoloading
;;

;;;###autoload
(rudel-backend-register 'transport
			'start-tls 'rudel-start-tls-backend)

(provide 'rudel-tls)
;;; rudel-tls.el ends here
