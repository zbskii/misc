;; A function for cycling through Haskell import lists.
;; Copyright (C) 2010 Chris Done <chrisdone@gmail.com>

;; The cycling step will stop once at the last import list so
;; that it is easy to add a new import list.

;; This module works completely independently of any libraries
;; (including haskell-mode).

;; Exports three interactive functions:
;; 1. haskell-navigate-imports
;; 2. haskell-goto-imports
;; 3. haskell-return-from-imports

;; Example usage:

;; (require 'haskell-navigate-imports)
;; (define-key haskell-mode-map [f8] 'haskell-navigate-imports)

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

(defvar haskell-went-to-imports-point nil)
(setq haskell-went-to-imports-point nil)

(defun haskell-navigate-imports (&optional return)
  "Cycle the Haskell import lines or return to point (with prefix arg)."
  (interactive "P")
  (if return
      (haskell-return-from-imports)
    (haskell-goto-imports)))

(defun haskell-goto-imports ()
  "Go to the first line of a list of consequtive import lines. Cycles."
  (interactive)
  (unless (or (haskell-import-line-match)
              (equal (line-beginning-position) (point-min))
              (save-excursion (previous-line)
                              (haskell-import-line-match)))
    (setq haskell-went-to-imports-point (point)))
  (haskell-goto-imports-internal))

(defun haskell-goto-imports-internal ()
  "Go to the first line of a list of consequtive import lines. Cycle."
  (if (haskell-import-line-match)
      (progn (haskell-goto-imports-end)
             (when (haskell-find-forward-import-line)
               (haskell-goto-imports-internal)))
    (let ((point (haskell-find-forward-import-line)))
      (if point
          (goto-char point)
        (progn (goto-char (point-min))
               (when (haskell-find-forward-import-line)
                 (haskell-goto-imports-internal)))))))

(defun haskell-return-from-imports ()
  "Return to the non-import point we were at before going to the module list.
   If we were originally at an import list, we can just cycle through easily."
  (interactive)
  (goto-char haskell-went-to-imports-point))

(defun haskell-goto-imports-end ()
  "Skip a bunch of consequtive import lines."
  (while (not (or (equal (point)
                         (point-max))
                  (not (haskell-import-line-match))))
    (forward-line)))

(defun haskell-find-forward-import-line ()
  "Return a point with at an import line, or nothing."
  (save-excursion
    (while (not (or (equal (point) (point-max))
                    (haskell-after-imports-p) ;; This one just speeds it up.
                    (haskell-import-line-match)))
      (forward-line))
    (let ((point (point)))
      (if (haskell-import-line-match)
          (point)
        nil))))

(defun haskell-import-line-match ()
  "Try to match the current line as a regexp."
  (let ((line (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position))))
    (if (string-match "^import " line)
        line
      nil)))

(defun haskell-after-imports-p ()
  "Are we after the imports list? Just for a speed boost."
  (save-excursion
    (goto-char (line-beginning-position))
    (not (not (search-forward-regexp "\\( = \\|\\<instance\\>\\| :: \\)"
                                     (line-end-position) t 1)))))

(provide 'haskell-navigate-imports)
