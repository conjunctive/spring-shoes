;;; spring-shoes.el --- quick imenu definition traversal -*- lexical-binding: t -*-

;; Copyright (C) 2020  Conjunctive

;; Author: Conjunctive <conjunctive@protonmail.com>
;; Keywords: imenu ivy
;; Version: 0.0.1
;; URL: https://github.com/conjunctive/spring-shoes
;; Package-Requires: ((emacs "26") cl-lib ivy)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'imenu)
(require 'subr-x)
(require 'thingatpt)
(require 'cl-lib)
(require 'ivy)

(defcustom spring-shoes-sock (lambda (_) (error "No identifier at point"))
  "Fallback unary function to call when 'spring-shoes is invoked and there is no identifier at point."
  :type 'function
  :group 'spring-shoes)

(defcustom spring-shoes-get-identifier 'word-at-point
  "Function to call for retrieving an identifier at point."
  :type 'function
  :group 'spring-shoes)

(defun filter-by-key (str alist)
  "Filter an ALIST by matching STR against the key of an item."
  (cl-remove-if-not (lambda (x)
                      (string-match-p (regexp-quote str) (car x)))
                    alist))

(defun tie-laces (category identifier)
  "Scan imenu index for IDENTIFIER under CATEGORY."
  (if-let ((index (or imenu--index-alist (imenu--make-index-alist t))))
      (if-let ((items (cdr (assoc category index))))
          (if-let ((result (assoc identifier items)))
              (list result)
            (if-let ((results (filter-by-key identifier items)))
                results
              (error "Unknown identifier: %s" identifier)))
        (error "Unknown category: %s" category))
    (error "Unable to retrieve menu buffer index")))

(defun jump (result)
  "Jump to a RESULT."
  (pcase result
    (`(,name ,pos ,fn . ,args)
     (push-mark nil t)
     (apply fn name pos args)
     (run-hooks 'imenu-after-jump-hook))
    (`(,name . ,pos)
     (push-mark nil t)
     (imenu-default-goto-function name pos)
     (run-hooks 'imenu-after-jump-hook))
    (x (error "Unknown imenu result: %s" x))))

(defun double-knot (category results)
  "Refine RESULTS under CATEGORY."
  (ivy-read (concat category ": ") results
            :action 'jump
            :caller 'double-knot))

;;;###autoload
(defun spring-shoes (category)
  "Jump to a matching identifier under CATEGORY."
  (interactive)
  (if (stringp category)
      (if-let ((identifier (funcall spring-shoes-get-identifier)))
          (let ((results (tie-laces category identifier)))
            (if (eq 1 (length results))
                (jump (car results))
              (double-knot category results)))
        (funcall spring-shoes-sock category))
      (error "No category specified")))

(provide 'spring-shoes)

;;; spring-shoes.el ends here
