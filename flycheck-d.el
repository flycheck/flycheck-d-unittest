;;; flycheck-d.el --- Add D support to flycheck

;; Copyright (C) 2013 by Tomoya Tanjo

;; Author: Tomoya Tanjo <ttanjo@gmail.com>
;; URL: https://github.com/tom-tan/flycheck-d/
;; Package-Requires: ((flycheck "20130606.1406") (dash "20130617.739"))
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
;;   * flycheck.el (https://github.com/lunaryorn/flycheck)
;;   * dash.el (https://github.com/magnars/dash.el)
;;
;; To use this package, add the following line to your .emacs file:
;;     (require 'flycheck-d)
;; It detects any compile errors, warnings and deprecated features.
;; And it also detects any errors during unit test.
;;
;; Note: flycheck-d runs DMD with -unittest and -main option for unittesting.
;; Please enclose main function in version(!unittest) block as follows:
;;
;; ---
;; import std.stdio;
;;
;; version(unittest) {}
;; else
;; void main()
;; {
;;     writeln("Hello!");
;; }
;;
;; unittest
;; {
;;     assert(1+2 == 3);
;; }
;; ---

;;; Code:
(require 'flycheck)
(require 'dash)
(require 's)

(defconst d-error-patterns
  '((error line-start (file-name) "(" line "): Error: " (message) line-end)
    (warning line-start (file-name) "(" line "): Warning: " (message) line-end)
    (warning line-start (file-name) "(" line "): Deprecation: " (message) line-end))
  "Error patterns for D.")

(defconst d-unittest-error-patterns
  '((error line-start (one-or-more anything) "@" (file-name) (zero-or-one ".d") "(" line "): " (message)))
  "Error patterns for D unittest.")

(defun flycheck-d-base-dir ()
  (let* ((str (buffer-string))
         (nest (if (string-match "module\s+\\([^\s]+\\);" str)
                   (->> str
                     (match-string-no-properties 1)
                     string-to-vector
                     (cl-count ?.))
                   0)))
    (when (equal (file-name-nondirectory (buffer-file-name)) "package.d")
      (cl-incf nest))
    (concat "-I./" (s-repeat nest "../"))))

(flycheck-define-checker d
  "A D syntax checker using D compiler."
  :command ("dmd" "-debug" "-o-" "-property" "-wi" (eval (flycheck-d-base-dir)) source)
  :error-patterns
  ((error line-start (file-name) "(" line "): Error: " (message) line-end)
   (warning line-start (file-name) "(" line "): Warning: " (message) line-end)
   (warning line-start (file-name) "(" line "): Deprecation: " (message) line-end))
  :modes d-mode
  :next-checkers ((warnings-only . d-unittest)))

(flycheck-define-checker d-unittest
  "A syntax and unittest checker for D."
  :command ("rdmd" "-debug" "-property" "-wi" (eval (flycheck-d-base-dir)) "-unittest" "-main" source)
  :error-parser
  (lambda (output _checker _buffer)
    (let* ((d-pat-regexp (--map (cons (flycheck-rx-to-string `(and ,@(cdr it)) :no-group)
                                    (car it))
                              d-error-patterns))
           (d-unittest-pat-regexp (--map (cons (flycheck-rx-to-string `(and ,@(cdr it)) :no-group)
                                             (car it))
                                       d-unittest-error-patterns))
           (tokens (flycheck-tokenize-output-with-patterns
                    output (append d-pat-regexp d-unittest-pat-regexp))))
      (-flatten
       (append (flycheck-parse-errors-with-patterns tokens d-pat-regexp)
               (mapcar (lambda (err)
                         (if (null err) err
                             (setf (flycheck-error-filename err)
                                   (concat (flycheck-error-filename err) ".d"))
                             err))
                       (flycheck-parse-errors-with-patterns tokens
                                                            d-unittest-pat-regexp))))))
  :modes d-mode)

(add-to-list 'flycheck-checkers 'd-unittest)
(add-to-list 'flycheck-checkers 'd)

(provide 'flycheck-d)
;;; flycheck-d.el ends here
