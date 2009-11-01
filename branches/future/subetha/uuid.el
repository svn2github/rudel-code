;;; uuid.el ---
;;
;; Author: Stefan Arentz
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
;; Defines new command, `uuid-generate-random-uuid', which will return
;; a randomly generated UUID as string.

(defun uuid-generate-random-uuid ()
  "Generate a random UUID."
  (mapconcat
   #'uuid--generate-random-hex-string
   (list 8 4 4 4 12) "-"))

(defun uuid--generate-random-hex-string (length)
  "Return a string consisting of LENGTH random hex chars.
  (let (result
	(digits "0123456789abcdef"))
    (dotimes (number length result)
      (push (elt digits (random 16)) result))
    (concat result)))

(provide 'uuid)
