;;; rudel-xml.el --- XML processing function used by Rudel
;;
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: rudel, xml
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
;; 0.1 - Initial revision


;;; Code:
;;

(require 'xml-parse) ;; TODO use xml.el which is distributed with Emacs


;;; Miscellaneous functions
;;

(defun xml->string (xml &optional pretty-print)
  "Convert infoset XML to string representation.
PRETTY-PRINT controls whether the resulting string is optimized
for readability or size."
  (with-temp-buffer
    (insert-xml xml pretty-print)
    (buffer-string)))

(defun string->xml (string)
  "Convert STRING to XML infoset."
  (with-temp-buffer
    (insert string)
    (goto-char 0)
    (cdr (xml-parse-read)))) ;; TODO this may be wrong


;;; Additional XML macros
;;

(defmacro with-tag-attrs (attrs tag &rest body)
  "Execute BODY with bindings of attribute values of TAG according to forms in ATTRS.
ATTRS is structured as follows:
ATTRS   ::= (BINDING*)
BINDING ::= VAR | (VAR ATTR) | (VAR ATTR TYPE)
VAR is a symbol. ATTR is a symbol whose symbol-name is used as
tag name. TYPE can be 'number."
  (declare (indent 2)
	   (debug (listp form &rest form)))
  (let* ((tag-var (make-symbol "tag-var"))
	 (bindings
	  (mapcar
	   (lambda (attr)
	     (cond
	      ;; Simple form
	      ((symbolp attr)
	       `(,attr (xml-tag-attr ,tag-var ,(symbol-name attr))))
	      ;; Variable name, attribute name and type
	      ((= (length attr) 3)
	       (let* ((attr-var     (nth 0 attr))
		      (name         (symbol-name (nth 1 attr)))
		      (type         (nth 2 attr))
		      (value-expr   `(xml-tag-attr ,tag-var ,name))
		      (value-string (make-symbol "value-string")))
		 `(,attr-var (let ((,value-string ,value-expr))
			       ,(cond
				 ((eq type 'number)
				  `(when ,value-string
				     (string-to-number ,value-string)))
				 (t value))))))
	      ;; Variable name and attribute name
	      ((= (length attr) 2)
	       (let* ((attr-var (nth 0 attr))
		      (name     (symbol-name (nth 1 attr)))
		      (value    `(xml-tag-attr ,tag-var ,name)))
		 `(,attr-var ,value)))
	      ;; Invalid form
	      (t (error "Invalid tag clause: %s" attr)))) ;; TODO define a proper condition or use signal?
	   attrs)))
    `(let ((,tag-var ,tag))
       (let (,@bindings)
	 (progn
	   ,@body))))
  )

(defmacro do-tag-children (var-and-tag &rest body)
  ""
  (declare (indent 1)
	   (debug ((symbolp form) &rest form)))
  (let ((var      (car  var-and-tag))
	(tag      (cadr var-and-tag))
	(children (make-symbol "children")))
    `(let ((,children (xml-tag-children ,tag)))
       (dolist (,var ,children)
	 ,@body)))
  )


;;; Stream-based parsing
;;

(defun rudel-xml-toplevel-tag-positions (string)
  ""
  (let ((depth       0)
	(tag-opening nil)
	(start)
	(tags        nil))
    (dolist (index (number-sequence 0 (- (length string) 1)))
      (cond
       ((= (aref string index) ?<)
	(setq tag-opening (/= (aref string (+ index 1)) ?/))
	(when (and (= depth 0)
		   tag-opening)
	  (setq start index)))

       ((= (aref string index) ?>)
	(unless (or (= (aref string (- index 1)) ?/)
		    (= (aref string (- index 1)) ??))
	  (if tag-opening
	      (incf depth)
	    (decf depth)))
	(when (= depth 0)
	  (push (cons start (+ index 1)) tags)))))

    (nreverse tags)))

(defun rudel-xml-toplevel-tags (string)
  ""
  (let ((tags (rudel-xml-toplevel-tag-positions string)))
    (list

     ;; Map top-level tag ranges into substrings.
     (mapcar
      (lambda (tag-range)
	(substring string (car tag-range) (cdr tag-range)))
      tags)

     ;; Add rest of the string
     (if tags
	 (substring string (apply #'max (mapcar #'cdr tags)))
       string)))
  )

(provide 'rudel-xml)


;;; Unit tests
;;

(eval-when-compile

  (require 'ert)

  (ert-deftest rudel-test-with-tag-attrs ()
    ""
    ;; Simple form
    (with-tag-attrs (a b c) '(("a" ("a" . "1") ("b" . "2") ("c" . "3")))
      (should (equal (list a b c) '("1" "2" "3"))))

    ;; List form without type
    (with-tag-attrs ((-a a) (-b b) (-c c))
	'(("a" ("a" . "1") ("b" . "2") ("c" . "3")))
      (should (equal (list -a -b -c) '("1" "2" "3"))))

    ;; List form with type
    (with-tag-attrs ((a a number) (b b number) (c c number))
	'(("a" ("a" . "1") ("b" . "2") ("c" . "3")))
      (should (equal (list a b c) '(1 2 3))))
    )

  (ert-deftest rudel-test-do-tag-children ()
    ""
    (let ((cases
	   '(((("a")) . nil)
	     ((("a") (("b")) (("c"))) . ((("b")) (("c")))))))
      (dolist (case cases)
	(destructuring-bind (tag . expected) case
	  (let ((children))
	    (do-tag-children (child tag)
	      (push child children))
	    (should (equal children expected))))))
    )

  (ert-deftest rudel-test-xml-toplevel-tag-positions ()
    ""
    (let ((cases
	   '((""                   . nil)
	     ("<a/>"               . ((0 . 4)))
	     ("<a><b/></a><c/>"    . ((0 . 11) (11 . 15)))
	     ("<a><b/><c/>"        . nil)
	     ("<a><b/></a><c><d/>" . ((0 . 11))))))
      (dolist (case cases)
	(destructuring-bind (data . expected) case
	  (should (equal
		   (rudel-xml-toplevel-tag-positions data)
		   expected)))))
    )

  (ert-deftest rudel-test-xml-toplevel-tags ()
    ""
    (let ((cases
	   '((""                   . (nil ""))
	     ("<a/>"               . (("<a/>") ""))
	     ("<a><b/></a><c/>"    . (("<a><b/></a>" "<c/>") ""))
	     ("<a><b/><c/>"        . (nil "<a><b/><c/>"))
	     ("<a><b/></a><c><d/>" . (("<a><b/></a>") "<c><d/>")))))
      (dolist (case cases)
	(destructuring-bind (data . expected) case
	  (should (equal
		   (rudel-xml-toplevel-tags data)
		   expected)))))
    )

;(let ((string "<hallo><bla/></hallo><hallo/><bla><blup></blup><bli>"))
;  (rudel-xml-toplevel-tags string))

;; (pp
;;  (macroexpand
;;   '(with-tag-attrs ((id id number)
;; 		    (bla bla number))
;;        '(("unsubscribe" ("id" . "0")))
;;      id))
;;  #'insert)

;; (with-tag-attrs ((id id number)
;; 		 (bla bla number))
;;        '(("unsubscribe" ("id" . "0")))
;;   (list id bla))

   )

;;; rudel-xml.el ends here
