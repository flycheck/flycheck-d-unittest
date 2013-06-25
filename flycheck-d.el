;;; flycheck-d.el --- Add D support to flycheck

;; Copyright (C) 2013 by Tomoya Tanjo

;; Author: Tomoya Tanjo <ttanjo@gmail.com>
;; URL: https://github.com/tom-tan/flycheck-d/
;; Package-Requires: ((dash "20130617.739"))
;; Keywords: flycheck, d

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library adds D support to flycheck.
;;
;; Requirements:
;;   * DMD 2.063 or later
;;   * dash.el (https://github.com/magnars/dash.el)
;;
;; To use this package, add the following line to your .emacs file:
;;     (require 'flycheck-d)
;; It detects any compile errors, warnings and deprecated features.
;; And it also detects any errors during unit test.

;;; Code:
(require 'flycheck)
(require 'dash)

(defconst d-error-patterns
  '(("^\\(?1:.*\\)(\\(?2:[0-9]+\\)): Error: \\(?4:.*\\)$" error)
    ("^\\(?1:.*\\)(\\(?2:[0-9]+\\)): Warning: \\(?4:.*\\)$" warning)
    ("^\\(?1:.*\\)(\\(?2:[0-9]+\\)): Deprecation: \\(?4:.*\\)$" warning))
  "Error patterns for D.")

(defconst d-unittest-error-patterns
  '(("^.+@\\(?1:[^.]+\\)\\(?:\\.d\\)?(\\(?2:[0-9]+\\)): \\(?4:.*\\)$" error))
  "Error patterns for D unittest.")

(defun flycheck-parse-d-unittest (output _checker _buffer)
  (let ((tokens (flycheck-tokenize-output-with-patterns output
                                                        (append d-error-patterns
                                                                d-unittest-error-patterns))))
    (-flatten
     (append (flycheck-parse-errors-with-patterns tokens d-error-patterns)
             (mapcar (lambda (err)
                       (if (null err) err
                           (setf (flycheck-error-filename err) (concat (flycheck-error-filename err) ".d"))
                           err))
                     (flycheck-parse-errors-with-patterns tokens d-unittest-error-patterns))))))

(flycheck-declare-checker d
  "A D syntax checker using D compiler."
  :command '("dmd" "-debug" "-o-" "-property" "-wi" source)
  :error-patterns d-error-patterns
  :modes 'd-mode
  :next-checkers '((warnings-only . d-unittest)))

(flycheck-declare-checker d-unittest
  "A syntax and unittest checker for D."
  :command '("dmd" "-debug" "-property" "-wi" "-unittest" "-main" "-run" source)
  :error-parser 'flycheck-parse-d-unittest
  :modes 'd-mode)

(add-to-list 'flycheck-checkers 'd-unittest)
(add-to-list 'flycheck-checkers 'd)

(provide 'flycheck-d)
;;; flycheck-d.el ends here
