;;; zeroconf.el --- Zeroconf support for Rudel
;;
;; Copyright (C) 2008, 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: Rudel, service, discovery, advertising, zeroconf,
;;           rendezvous, avahi
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
;; Zeroconf session discovery and advertising for Rudel. The main
;; class `rudel-zeroconf-backend' implements discovery and advertising
;; for registered service types. Service types are registered by
;; adding an element of the form (SERVICE BACKEND) to the
;; `rudel-zeroconf-service-types'. BACKEND is the symbol of the
;; protocol backend and SERVICE is the string used as service type in
;; the Zeroconf record (example: '("_lobby._tcp" obby)).


;;; History:
;;
;; 0.1 - Initial revision.


;;; Code:
;;

(eval-when-compile
  (require 'cl)) ;; first, second

(require 'zeroconf)

(require 'rudel/backend)
(require 'rudel/session-initiation)


;;; Constants and global variables
;;

(defconst rudel-zeroconf-version '(0 1)
  "Version of the Zeroconf backend for Rudel.")

(defvar rudel-zeroconf-service-types nil
  "Service types used by Rudel backends.
Each element is of the form (SERVICE BACKEND).")


;;; Accessors and manipulators for the service list
;;

(defalias 'rudel-zeroconf-service-type 'first
  "Return type of service.")

(defalias 'rudel-zeroconf-service-backend 'second
  "Return backend associated with service type.")

(defun rudel-zeroconf-service (key which)
  "Return the Zeroconf service type used by BACKEND."
  (find which rudel-zeroconf-service-types
	:key key :test (if (eq key 'rudel-zeroconf-service-type)
			   #'string= #'eq)))

;;;###autoload
(defun rudel-zeroconf-register-service (type backend)
  "Add an entry for TYPE with BACKEND to the list of service types.
BACKEND is the name of the protocol backend handling the service
type TYPE."
  (push (list type backend)
	rudel-zeroconf-service-types))


;;; Initialization
;;

(message "Initializing Zeroconf ...")
(zeroconf-init)


;;; Class rudel-zeroconf-backend
;;

;;;###autoload
(defclass rudel-zeroconf-backend (rudel-session-initiation-backend)
  ((capabilities :initform (discover advertise))
   (priority     :initform primary))
  "")

(defmethod initialize-instance ((this rudel-zeroconf-backend) &rest slots)
  "Initialize slots of THIS with SLOTS."
  (when (next-method-p)
    (call-next-method))

  (oset this :version rudel-zeroconf-version))

(defmethod rudel-discover ((this rudel-zeroconf-backend))
  "Return a list of session information property lists for Zeroconf-advertised sessions."
  (mapcar
   #'rudel-zeroconf-service->plist
   (remove-if
    #'null
    (mapcar
     #'zeroconf-resolve-service
     (apply
      #'append
      (mapcar
       #'rudel-zeroconf-services
       (mapcar #'rudel-zeroconf-service-backend
	       rudel-zeroconf-service-types))))))
  )

(defmethod rudel-advertise ((this rudel-session-initiation-backend) info)
  "Use Zeroconf to advertise the session described by INFO to other users."
  (let ((name    (plist-get info :name))
	(backend (plist-get info :backend))
	(host    (plist-get info :host))
	(port    (plist-get info :port))
	(data    (plist-get info :data)))
    (when backend
      (apply #'rudel-zeroconf-publish
	     backend name host port data)))
  t)

(defmethod rudel-withdraw ((this rudel-session-initiation-backend))
  "Withdraw Zeroconf record."
  (error "Not implemented, yet"))


;;; Zeroconf wrapper functions
;;

(defun rudel-zeroconf-services (backend)
  "List Zeroconf services for BACKEND."
  (zeroconf-list-services
   (rudel-zeroconf-service-type
    (rudel-zeroconf-service
     #'rudel-zeroconf-service-backend backend))))

(defun rudel-zeroconf-services-present-p (backend)
  "Check whether there are Zeroconf services for BACKEND."
  (rudel-zeroconf-services backend))

(defun rudel-zeroconf-publish (backend name host port &rest data)
  "Publish BACKEND service NAME for HOST and PORT."
  ;; Try to find the service entry for the protocol backend and
  ;; publish the service if an entry is found.
  (let ((service (rudel-zeroconf-service
		  #'rudel-zeroconf-service-backend backend)))
    (when service
      (zeroconf-publish-service
       name
       (rudel-zeroconf-service-type service)
       "local"
       (concat host ".local")
       port
       "" ; address
       (mapcar
	(lambda (item)
	  (concat (car item) "=" (cdr item)))
	data)
       )))
  )

(defun rudel-zeroconf-withdraw (backend name)
  "Withdraw service NAME for BACKEND."
  (error "Not implemented, yet"))


;;; Miscellaneous functions
;;

(defun rudel-zeroconf-service->plist (service)
  "Convert a Zeroconf service record to an info property list."
  (let* ((type         (zeroconf-service-type service))
	 (data         (rudel-zeroconf-parse-txt-record
			(zeroconf-service-txt service)))
	 (service-type (rudel-zeroconf-service
			#'rudel-zeroconf-service-type type)))
    ;; Construct information property list.
    (list
     :name       (format "Zeroconf advertised session \"%s\""
			 (zeroconf-service-name service))
     :backend    (rudel-backend-get
		  'protocol
		  (rudel-zeroconf-service-backend service-type))
     :host       (zeroconf-service-host service)
     :port       (zeroconf-service-port service)
     ;; Encryption defaults to yes to be compatible with Gobby.
     :encryption (or (not (member :encryption data))
		     (string= (plist-get data :encryption)
			      "yes"))))
  )

(defun rudel-zeroconf-parse-txt-record (record)
  "Parse RECORD into a property list of keys and values."
  (apply #'append
	 (mapcar
	  (lambda (entry)
	    (multiple-value-bind (key value) (split-string entry "=")
	      (list (intern (concat ":" key))
		    value)))
	  record))
  )


;;; User interaction
;;

(defun rudel-zeroconf-read-service (backend)
  "Retrieve services for BACKEND and read one from user."
  ;; First, find all matching services for the backend.
  (let* ((services         (rudel-zeroconf-services backend))
	 ;; Read one of the names of these services.
	 (service-name     (completing-read
			    "Service: "
			    (mapcar #'zeroconf-service-name services)
			    nil t))
	 ;; Retrieve and resolve the selected service object.
	 (service          (find service-name services
				 :key  #'zeroconf-service-name
				 :test #'string=))
	 (service-resolved (zeroconf-resolve-service service)))
    ;; Return host and port
    (list (zeroconf-service-host service-resolved)
	  (zeroconf-service-port service-resolved)))
  )


;;; Autoloading
;;

;;;###autoload
(rudel-add-backend (rudel-backend-get-factory 'session-initiation)
		   'zeroconf 'rudel-zeroconf-backend)

(provide 'rudel/zeroconf/zeroconf)
;;; zeroconf.el ends here
