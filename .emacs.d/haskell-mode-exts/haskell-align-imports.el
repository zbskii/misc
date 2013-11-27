;; Align the import lines in a Haskell file.
;; Copyright (C) 2010 Chris Done <chrisdone@gmail.com>

;; This module is intended for Haskell mode users, but is
;; independent of Haskell mode.

;; Example usage:

;; (require 'haskell-align-imports)
;; (define-key haskell-mode-map (kbd "C-c .") 'haskell-align-imports)

;; Consider the following imports list:
;;
;; import One
;; import Two as A
;; import qualified Three
;; import qualified Four as PRELUDE
;; import Five (A)
;; import Six (A,B)
;; import qualified Seven (A,B)
;; import "abc" Eight
;; import "abc" Nine as TWO
;; import qualified "abc" Ten
;; import qualified "defg" Eleven as PRELUDE
;; import "barmu" Twelve (A)
;; import "zotconpop" Thirteen (A,B)
;; import qualified "z" Fourteen (A,B)
;; import Fifteen hiding (A)
;; import Sixteen as TWO hiding (A)
;; import qualified Seventeen hiding (A)
;; import qualified Eighteen as PRELUDE hiding (A)
;; import "abc" Nineteen hiding (A)
;; import "abc" Twenty as TWO hiding (A)
;;
;; When haskell-align-imports is run within the same buffer, the
;; import list is transformed to:
;;
;; import                  One
;; import                  Two       as A
;; import qualified        Three
;; import qualified        Four      as PRELUDE
;; import                  Five      (A)
;; import                  Six       (A,B)
;; import qualified        Seven     (A,B)
;; import "abc"            Eight
;; import "abc"            Nine      as TWO
;; import qualified "abc"  Ten
;; import qualified "defg" Eleven    as PRELUDE
;; import "barmu"          Twelve    (A)
;; import "zotconpop"      Thirteen  (A,B)
;; import qualified "z"    Fourteen  (A,B)
;; import                  Fifteen   hiding (A)
;; import                  Sixteen   as TWO hiding (A)
;; import qualified        Seventeen hiding (A)
;; import qualified        Eighteen  as PRELUDE hiding (A)
;; import "abc"            Nineteen  hiding (A)
;; import "abc"            Twenty    as TWO hiding (A)

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

(require 'cl)

;; Regular expression for matching an import line.
(defvar *haskell-import-line-regexp* "")
(setq *haskell-import-line-regexp*
      ;; Broken up into its constituent groups:
      (concat "^\\(import[ ]+\\)"
              "\\(qualified \\)?"
              "[ ]*\\(\"[^\"]*\" \\)?"
              "[ ]*\\([A-Za-z0-9_.']*\\)"
              "[ ]*\\([ ]*as [A-Z][^ ]*\\)?"
              "[ ]*\\((.*)\\)?"
              "\\([ ]*hiding (.*)\\)?"
              "\\( -- .*\\)?[ ]*$"))

(defun haskell-align-imports ()
  "Align all the imports in the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((imports (haskell-collect-imports))
           (padding (haskell-imports-padding imports)))
      (mapc (lambda (x)
              (goto-char (cdr x))
              (delete-region (point) (line-end-position))
              (insert (haskell-import-chomp (haskell-fill-import padding (car x)))))
            imports)))
  nil)

(defun haskell-collect-imports ()
  "Collect a list of mark / import statement pairs."
  (let ((imports '()))
    (while (not (or (equal (point) (point-max)) (haskell-after-imports-p)))
      (let ((line (haskell-import-line-match-it)))
        (when line
          (let ((match (haskell-merge-import-parts
                        (loop for i from 1 to 8
                              collect (haskell-import-chomp (match-string i line))))))
            (setq imports (cons (cons match (line-beginning-position))
                                imports)))))
      (forward-line))
    imports))

(defun haskell-merge-import-parts (l)
  "Merge together parts of an import statement that shouldn't be separated."
  (let ((parts (apply #'vector l))
        (join (lambda (ls)
                (reduce (lambda (a b)
                          (concat a
                                  (if (and (> (length a) 0)
                                           (> (length b) 0))
                                      " "
                                    "")
                                  b))
                        ls))))
    (list (funcall join (list (aref parts 0)
                              (aref parts 1)
                              (aref parts 2)))
          (aref parts 3)
          (funcall join (list (aref parts 4)
                              (aref parts 5)
                              (aref parts 6)))
          (aref parts 7))))

(defun haskell-import-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (if str
      (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" ""
                                str)
    ""))

(defun haskell-imports-padding (imports)
  "Find the padding for each part of the import statements."
  (reduce (lambda (a b) (mapcar* #'max a b))
          (mapcar (lambda (x) (mapcar #'length (car x)))
                  imports)))

(defun haskell-fill-import (padding line)
  "Fill an import line using the padding worked out from all statements."
  (mapconcat #'identity
             (mapcar* (lambda (pad part)
                        (if (> (length part) 0)
                            (concat part (make-string (- pad (length part)) ? ))
                          (make-string pad ? )))
                      padding
                      line)
             " "))

(defun haskell-import-line-match-it ()
  "Try to match the current line as a regexp."
  (let ((line (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position))))
    (if (string-match *haskell-import-line-regexp* line)
        line
      nil)))

(defun haskell-after-imports-p ()
  "Are we after the imports list?"
  (save-excursion
    (goto-char (line-beginning-position))
    (not (not (search-forward-regexp "\\( = \\|\\<instance\\>\\| :: \\)"
                                     (line-end-position) t 1)))))

(provide 'haskell-align-imports)
