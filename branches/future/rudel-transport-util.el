;;; rudel-transport-util.el --- Utility functions for Rudel transport functionality
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: rudel, backend, transport, utility, miscellaneous
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
;; This file contains utility functions for implementing Rudel
;; transport functionality. In particular, several transport filter
;; classes for common task are available:
;;
;; + `rudel-transport-filter'
;;   + `rudel-assembling-transport-filter'
;;   + `rudel-parsing-transport-filter'
;;   + `rudel-progress-reporting-transport-filter'


;;; History:
;;
;; 0.2 - Progress reporting transport filter
;;
;; 0.1 - Initial revision


;;; Code:
;;

(require 'rudel-transport)


;;; Class rudel-transport-filter
;;

(defclass rudel-transport-filter (rudel-transport)
  ((transport :initarg  :transport
	      :type     rudel-transport
	      :documentation
	      "Transport object that performs the send and
receive operations this filter builds upon.")  ;; TODO should be read only
   (filter    :initarg  :filter
	      :type     (or null function)
	      :initform nil
	      :reader   rudel-filter
	      :writer   rudel-set-filter
	      :documentation
	      "Function that is called when data is received.")
   (sentinel  :initarg  :sentinel
	      :type     (or null function)
	      :initform nil
	      :reader   rudel-sentinel
	      :writer   rudel-set-sentinel
	      :documentation
	      "Function that is called when the status of the
transport changes."))
  "This class is a base class for transport filters that
transform a bidirectional data stream as it passes through them."
  :abstract t)

(defmethod slot-missing ((this rudel-transport-filter)
			 slot-name operation &optional new-value)
  "Make slots of underlying transport available as virtual slots of THIS."
  (cond
   ((eq operation 'oref)
    (slot-value (oref this :transport) slot-name))

   ((eq operation 'oset)
    (set-slot-value (oref this :transport) slot-name new-value)))
  )

(defmethod no-applicable-method ((this rudel-transport-filter)
				 method &rest args)
  "Make methods of underlying transport callable as virtual methods of THIS."
  (apply method (oref this :transport) (rest args)))


;;; Class rudel-assembling-transport-filter
;;

(defclass rudel-assembling-transport-filter (rudel-transport-filter)
  ((buffer            :initarg  :buffer
		      :type     list
		      :initform nil
		      :documentation
		      "Stores message fragments until complete
messages can be assembled.")
   (assembly-function :initarg  :assembly-function
		      :type     function
		      :reader   rudel-assembly-function
		      :writer   rudel-set-assembly-function
		      :documentation
		      "Function that is called to assemble
message fragments into complete messages."))
  "Objects of this class assemble received message fragments into
complete messages by calling an assembly function.")

(defmethod initialize-instance ((this rudel-assembling-transport-filter)
				slots)
  "Initialize THIS using SLOTS and install suitable handlers."
  ;; Initialize slots.
  (when (next-method-p)
    (call-next-method))

  ;; Install a handler for received data that assembles messages and
  ;; passes them to the user-provided handler.
  (with-slots (transport) this
    (lexical-let ((this1 this))
      (rudel-set-filter
       transport
       (lambda (data)

	 ;; Assemble complete fragments from stored fragments and
	 ;; possibly incomplete messages in DATA.
	 (with-slots (buffer assembly-function) this1
	   (rudel-assemble-fragments data buffer assembly-function))

	 ;; Process all complete messages.
	 (with-slots (filter) this1
	   (when filter
	     (mapc filter data)))))))
  )

(defmethod rudel-send ((this rudel-assembling-transport-filter) data)
  "Send DATA using the transport of THIS."
  (with-slots (transport) this
    (rudel-send transport data)))


;;; Class rudel-parsing-transport-filter
;;

(defclass rudel-parsing-transport-filter (rudel-transport-filter)
  ((parse-function    :initarg  :parse-function
		      :type     function
		      :initform 'identity
		      :documentation
		      "Function that is called on each received
piece of data to transform it into a suitable representation.")
   (generate-function :initarg  :generate-function
		      :type     function
		      :initform 'identity
		      :documentation
		      "Function that is called on each outgoing
object to transform it into a string representation."))
  "Objects of this class convert sent and received data between
string representations and structured representations by calling
a pair of one parse and one generate function.")

(defmethod initialize-instance ((this rudel-parsing-transport-filter) slots)
  "Initialize THIS using SLOTS and install suitable handlers."
  ;; Initialize slots.
  (when (next-method-p)
    (call-next-method))

  ;; Install a handler for received data that parses messages into
  ;; structured representations and passes those to the user-provided
  ;; handler.
  (with-slots (transport) this
    (lexical-let ((this1 this))
      (rudel-set-filter
       transport
       (lambda (message-data)
	 ;; Parse and process all complete messages.
	 (with-slots (parse-function filter) this1
	   (when filter
	     (let ((message (funcall parse-function message-data)))
	       (funcall filter message))))))))
  )

(defmethod rudel-send ((this rudel-parsing-transport-filter) message)
  "Apply generate function to MESSAGE, pass result to transport of THIS."
  (with-slots (transport generate-function) this
    (rudel-send transport (funcall generate-function message))))


;;; Class rudel-progress-reporting-transport-filter
;;

;; TODO have a callback instead of the actual reporter
(defclass rudel-progress-reporting-transport-filter (rudel-transport-filter)
  ((reporter :initarg :reporter
	     :documentation
	     "TODO"))
  "TODO")

(defmethod initialize-instance
  ((this rudel-progress-reporting-transport-filter) slots)
  "TODO"
  (when (next-method-p)
    (call-next-method))

  (with-slots (reporter) this
    (setq reporter (make-progress-reporter "Sending data " 0.0 1.0)))
  )

(defmethod rudel-send ((this rudel-progress-reporting-transport-filter)
		       data)
  "TODO"
  (with-slots (transport reporter) this
    (if (>= (length data) rudel-long-message-threshold)

	;; For huge messages, chunk the message data and transmit the
	;; chunks
	(let ((total    (/ (length data)
			   rudel-long-message-chunk-size))
	      (current  0))
	  (rudel-loop-chunks data chunk rudel-long-message-chunk-size
	    (progress-reporter-update reporter (/ (float current) total))
	    (rudel-send transport socket chunk)
	    (incf current))
	  (progress-reporter-done reporter))

      ;; Send small messages in one chunk
      (rudel-send transport data)))
  )


;;; Miscellaneous functions
;;

(defun rudel-transport-make-filter-stack (base specs)
  "Construct a filter stack on top of BASE according to SPECS.

SPECS is structured as follows:
SPECS ::= (SPEC*)
SPEC  ::= (CLASS KWARG*)
KWARG ::= KEYWORD VALUE
CLASS is the symbol of a class derived from
`rudel-transport-filter' KEYWORD is a keyword and VALUE is an
arbitrary expression and is used unevaluated.

The returned value is the \"top\" of the constructed stack (BASE
being the \"bottom\")."
  (let ((current base))
    (dolist (spec specs)
      (destructuring-bind (class &rest args) spec
	  (setq current (apply #'make-instance
			       class
			       :transport current
			       args))))
    current))

(provide 'rudel-transport-util)


;;; Unit tests
;;

(eval-when-compile

  (require 'ert )

  ;; Dummy transport class for tests

  (defclass rudel-dummy-transport (rudel-transport)
    ((test :initarg :test)))

  (defmethod rudel-set-filter ((this rudel-dummy-transport) filter)
    )

  ;; Test cases

  (ert-deftest rudel-transport-util-test-transport-filter ()
    "Tests for class `rudel-transport-filter'.
In fact, `rudel-assembling-transport-filter' is used for the test
since `rudel-transport-filter' is abstract. However, only
properties of `rudel-transport-filter' are tested."
    (let* ((transport (rudel-dummy-transport
		       "bla"
		       :test 'test))
	   (filter    (rudel-assembling-transport-filter
		       "bla"
		       :transport transport)))
      (should       (eq (oref filter :transport) transport))
      (should       (eq (oref filter :test) 'test))
      (should-error (oref filter :invalid)
		    :type 'invalid-slot-name))
    )

  (ert-deftest rudel-transport-util-test-make-filter-stack ()
    "Tests for `rudel-transport-make-filter-stack'."
    (let* ((transport (rudel-dummy-transport
		       "bla"
		       :test 'test))
	   (top       (rudel-transport-make-filter-stack
		       transport
		       '((rudel-progress-reporting-transport-filter
			  :reporter nil)))))
      (should (eq (oref top :transport) transport))
      (should (eq (object-class top)
		  rudel-progress-reporting-transport-filter)))
    )

  )

;;; rudel-transport-util.el ends here
