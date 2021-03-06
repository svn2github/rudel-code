;;; rudel-xmpp-sasl.el --- SASL mechanism for the Rudel XMPP backend
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, xmpp, sasl, authentication
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
;; 0.1 - Initial version


;;; Code:
;;

(require 'base64)
(require 'sasl)

(require 'eieio)

(require 'rudel-xmpp-state)


;;; Class rudel-xmpp-state-sasl-start
;;

(defclass rudel-xmpp-state-sasl-start (rudel-xmpp-state)
  ()
  "Start state of the SASL negotiation.")

(defmethod rudel-enter ((this rudel-xmpp-state-sasl-start)
			name server features)
  "Extract the list of supported mechanisms from FEATURES.
Then switch to the try one state to try them in order."
  ;; Find mechanism tags
  (let* ((mechanism-tags (remove* "mechanisms" features
				  :test-not #'string=
				  :key      #'xml-tag-name))
	 ;; XML -> alist
	 (mechanisms
	  (apply #'append
		 (mapcar
		  (lambda (mechanisms)
		    (let ((schema (or (xml-tag-attr mechanisms "xmlns")
				      "urn:ietf:params:xml:ns:xmpp-sasl")))
		      (mapcar
		       (lambda (mechanism)
			 (list schema
			       (car (xml-tag-children mechanism))))
		       (xml-tag-children mechanisms))))
		  mechanism-tags))))

    ;; Try the first mechanism
    (list 'sasl-try-one name server mechanisms))
  )


;;; Class rudel-xmpp-state-sasl-try-one
;;

(defclass rudel-xmpp-state-sasl-try-one (rudel-xmpp-state)
  ()
  "State that selects a mechanism and switches to the mechanism
start state for that mechanism.")

(defmethod rudel-enter ((this rudel-xmpp-state-sasl-try-one)
			name server mechanisms)
  "If Emacs support the first mechanism in MECHANISMS, try it, otherwise skip it.
Mechanism are tried by switching to the mechanism start state.
When no mechanisms are left, switch to the authentication failed state."
  ;; If there are mechanism on the list, try them, otherwise fail.
  (if mechanisms
      (destructuring-bind (schema mechanism-name) (car mechanisms)
	;; If Emacs supports the head of the mechanism list, try it,
	;; otherwise go with the tail.
	(let ((mechanism (sasl-find-mechanism (list mechanism-name))))
	  (if mechanism
	      (list 'sasl-mechanism-start
		    name server schema mechanism (cdr mechanisms))
	    (list 'sasl-try-one name server (cdr mechanisms)))))
    'authentication-failed)
  )


;;; Class rudel-xmpp-state-sasl-mechanism-start
;;

(defclass rudel-xmpp-state-sasl-mechanism-start (rudel-xmpp-state)
  ((schema    :initarg :schema
	      :type    string
	      :documentation
	      "")
   (mechanism :initarg :mechanism
	      :type    vector
	      :documentation
	      "")
   (rest      :initarg :rest
	      :type    list
	      :documentation
	      ""))
  "Start state of the negotiation for the selected mechanism.")

(defmethod rudel-enter ((this rudel-xmpp-state-sasl-mechanism-start)
			name1 server1 schema1 mechanism1 rest1)
  ""
  (with-slots (schema mechanism rest) this
    (setq schema    schema1
	  mechanism mechanism1
	  rest      rest1)

    (let* ((client (sasl-make-client mechanism name1 "xmpp" server1))
	   (step   (sasl-next-step client nil))
	   (name   (sasl-mechanism-name mechanism)))

      ;; Send initial 'auth' message.
      (rudel-send this
		  `(("auth"
		     ("xmlns"     . ,schema)
		     ("mechanism" . ,name))))

      ;; Construct the initial SASL step for the mechanism and start
      ;; the challenge/response sequence.
      (list 'sasl-mechanism-step name1 server1 schema client step rest)))
  )


;;; Class rudel-xmpp-state-sasl-mechanism-step
;;

(defclass rudel-xmpp-state-sasl-mechanism-step (rudel-xmpp-state)
  ((name   :initarg :name
	   :type    string
	   :documentation
	   "Username used in SASL authentication mechanism.")
   (server :initarg :server
	   :type    string
	   :documentation
	   "Server name used in SASL authentication mechanism.")
   (schema :initarg :schema
	   :type    string
	   :documentation
	   "Schema URN identifying the SASL mechanism.")
   (client :initarg :client
	   :type    vector
	   :documentation
	   "SASL mechanism data.")
   (step   :initarg :step
	   :type    vector
	   :documentation
	   "SASL mechanism state data.")
   (rest   :initarg :rest
	   :type    list
	   :documentation
	   "List of remaining mechanisms to try."))
  "Intermediate step of the negotiation for the selected
mechanism.")

(defmethod rudel-enter ((this rudel-xmpp-state-sasl-mechanism-step)
			name1 server1 schema1 client1 step1 rest1)
  "Store SCHEMA1, CLIENT1, STEP1 and REST1 for later use."
  (with-slots (name server schema client step rest) this
    (setq name   name1
	  server server1
	  schema schema1
	  client client1
	  step   step1
	  rest   rest1))
  nil)

(defmethod rudel-accept ((this rudel-xmpp-state-sasl-mechanism-step) xml)
  "Interpret XML to decide how to proceed with the authentication mechanism."
  (cond
   ;; Authentication mechanism failed. Try next.
   ((string= (xml-tag-name xml) "failure")
    (let ((child (car (xml-tag-children xml))))
      (cond
       ;; The not-authorized failure means that the credentials we
       ;; provided were wrong.
       ((string= (xml-tag-name child) "not-authorized")
	(with-slots (name server rest) this
	  (list 'sasl-try-one name server rest)))

       ;; Default behavior is to try next mechanism.
       (t
	(with-slots (name server rest) this
	  (list 'sasl-try-one name server rest))))))

   ;; Authentication mechanism succeeded. Switch to authenticated
   ;; state.
   ((string= (xml-tag-name xml) "success")
    'authenticated)

   ;; Authentication mechanism requires a challenge-response step. The
   ;; Emacs SASL implementation does the heavy lifting for us.
   ((string= (xml-tag-name xml) "challenge")
    ;; TODO is the challenge data always there?
    (with-slots (name server schema client step rest) this
      ;; TODO assert string= schema (xml-tag-attr xml "xmlns")

      ;; Pass challenge data, if any, to current step.
      (when (stringp (car-safe (xml-tag-children xml)))
	(let ((challenge-data (base64-decode-string
			       (car (xml-tag-children xml)))))
	  (sasl-step-set-data step challenge-data)))

      ;; Proceed to next step and send response.
      (setq step (sasl-next-step client step))
      (let* ((response-data-raw (sasl-step-data step))
	     (response-data     (when response-data-raw
				  (base64-encode-string
				   response-data-raw t))))
	(rudel-send this
		    `(("response"
		       ,@(when schema
			   (list (cons "xmlns" schema))))
		      ,@(when response-data
			  (list response-data)))))

      (list 'sasl-mechanism-step name server schema client step rest)))

   ;; Unknown message.
   (t
    nil)) ;; TODO send error or call-next-method?
  )

;; 6.4.  SASL Errors
;;
;;    The following SASL-related error conditions are defined:
;;
;;    o  <aborted/> -- The receiving entity acknowledges an <abort/>
;;       element sent by the initiating entity; sent in reply to the
;;       <abort/> element.
;;
;;    o  <incorrect-encoding/> -- The data provided by the initiating
;;       entity could not be processed because the [BASE64] encoding is
;;       incorrect (e.g., because the encoding does not adhere to the
;;       definition in Section 3 of [BASE64]); sent in reply to a
;;       <response/> element or an <auth/> element with initial response
;;       data.
;;
;;    o  <invalid-authzid/> -- The authzid provided by the initiating
;;       entity is invalid, either because it is incorrectly formatted or
;;       because the initiating entity does not have permissions to
;;       authorize that ID; sent in reply to a <response/> element or an
;;       <auth/> element with initial response data.
;;
;;    o  <invalid-mechanism/> -- The initiating entity did not provide a
;;       mechanism or requested a mechanism that is not supported by the
;;       receiving entity; sent in reply to an <auth/> element.
;;
;;    o  <mechanism-too-weak/> -- The mechanism requested by the initiating
;;       entity is weaker than server policy permits for that initiating
;;       entity; sent in reply to a <response/> element or an <auth/>
;;       element with initial response data.
;;
;;    o  <not-authorized/> -- The authentication failed because the
;;       initiating entity did not provide valid credentials (this includes
;;       but is not limited to the case of an unknown username); sent in
;;       reply to a <response/> element or an <auth/> element with initial
;;       response data.
;;
;;    o  <temporary-auth-failure/> -- The authentication failed because of
;;       a temporary error condition within the receiving entity; sent in
;;       reply to an <auth/> element or <response/> element.


;;; SASL state list
;;

(defvar rudel-xmpp-sasl-states
  '((sasl-start           . rudel-xmpp-state-sasl-start)
    (sasl-try-one         . rudel-xmpp-state-sasl-try-one)
    (sasl-mechanism-start . rudel-xmpp-state-sasl-mechanism-start)
    (sasl-mechanism-step  . rudel-xmpp-state-sasl-mechanism-step))
  "States used during SASL authentication.")

(eval-after-load "rudel-xmpp"
  '(dolist (state rudel-xmpp-sasl-states)
     (add-to-list 'rudel-xmpp-states state)))

(provide 'rudel-xmpp-sasl)
;;; rudel-xmpp-sasl.el ends here
