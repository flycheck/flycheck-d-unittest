;;; flycheck-d-unittest.el --- Add D unittest support to flycheck

;; Copyright (C) 2013 by Tomoya Tanjo

;; Author: Tomoya Tanjo <ttanjo@gmail.com>
;; URL: https://github.com/tom-tan/flycheck-d-unittest/
;; Package-Requires: ((flycheck "0.14") (dash "1.4.0"))
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

;; This library adds D unittest support to flycheck.
;;
;; Requirements:
;;   * DMD 2.063 or later
;;   * flycheck.el (https://github.com/lunaryorn/flycheck)
;;   * dash.el (https://github.com/magnars/dash.el)
;;
;; To use this package, add the following line to your .emacs file:
;;     (require 'flycheck-d-unittest)
;;     (setup-flycheck-d-unittest)
;; It detects any compile errors, warnings and deprecated features during unittest.
;;
;; Note: flycheck-d-unittest runs DMD with -unittest and -main option for unittesting.
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

(defconst d-dmd-unittest-error-patterns
  '((error line-start (one-or-more anything) "@" (file-name) (zero-or-one ".d") "(" line "): " (message)))
  "Error patterns for D unittest using the DMD compiler.")

(flycheck-define-checker d-dmd-unittest
  "A D syntax and unittest checker using the DMD compiler."
  :command ("rdmd" "-debug" "-property" "-wi"
                   (eval (s-concat "-I" (flycheck-d-base-directory)))
                   "-unittest" "-main" source)
  :error-parser
  (lambda (output _checker _buffer)
    (let* ((d-checker-regexp (flycheck-checker-error-patterns 'd-dmd))
           (unittest-pat-regexp (--map (cons (flycheck-rx-to-string `(and ,@(cdr it)) :no-group)
                                             (car it))
                                       d-dmd-unittest-error-patterns))
           (tokens (flycheck-tokenize-output-with-patterns
                    output (append d-checker-regexp unittest-pat-regexp))))
      (-flatten
       (append (flycheck-parse-errors-with-patterns tokens d-checker-regexp)
               (mapcar (lambda (err)
                         (when err
                           (setf (flycheck-error-filename err)
                                 (concat (flycheck-error-filename err) ".d"))
                           err))
                       (flycheck-parse-errors-with-patterns tokens
                                                            unittest-pat-regexp))))))
  :modes d-mode)

;;;###autoload
(defun setup-flycheck-d-unittest ()
  "Set up for flycheck D unittest checkers."
  (add-to-list 'flycheck-checkers 'd-dmd-unittest)
  (put 'd-dmd :flycheck-next-checkers
       (cons '(warnings-only . d-dmd-unittest) (flycheck-checker-next-checkers 'd-dmd))))

(provide 'flycheck-d-unittest)
;;; flycheck-d-unittest.el ends here
