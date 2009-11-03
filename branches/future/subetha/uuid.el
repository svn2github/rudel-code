;;; uuid.el ---
;;
;; Copyright (C) ? Stefan Arentz
;; Copyright (C) 2009 Jan Moringen
;;
;; Author: Stefan Arentz
;;         Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: uuid, unique, id, random
;; X-RCS: $Id:$
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:
;;
;; UUID specification: http://www.ietf.org/rfc/rfc4122.txt
;;
;; Defines new command, `uuid-generate-random-uuid', which will return
;; a randomly generated UUID as string.


;;; Operations on UUIDs
;;

(defun uuid= (left right)
  ""
  (string= (downcase left) (downcase right)))


;;; Generating UUIDs
;;

(defun uuid-generate-random-uuid ()
  "Generate a random UUID and return it as string."
  (mapconcat
   #'uuid--generate-random-hex-string
   '(8 4 4 4 12) "-"))

(defun uuid--generate-random-hex-string (length)
  "Return a string consisting of LENGTH random hex chars."
  (let ((result (make-string length ?_))
	(digits "0123456789abcdef"))
    (dotimes (i length result)
      (aset result i (aref digits (random 16))))))

(provide 'uuid)


;;; Unit tests
;;

(eval-when-compile
  (when (require 'ert nil t)

    (ert-deftest uuid-test-uuid= ()
      ""
      (let ((a (uuid-generate-random-uuid))
	    (b (uuid-generate-random-uuid)))
	(and (uuid= a a)
	     (uuid= b b)
	     (not (uuid= a b))
	     (not (uuid= b a)))))

    ))

;; uuid.el ends here
