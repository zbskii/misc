;; A function for observing the currently installed Haskell
;; packages.
;; Copyright (C) 2010 Chris Done <chrisdone@gmail.com>

;; This module is intended for Elisp programmers to use in other
;; packages that involve inspecting the currently installed
;; packages and modules.

;; Example usage:
;;
;; (require 'haskell-installed-packages)
;; (setq haskell-ghc-pkg-bin-path "/home/chris/Programs/bin/ghc-pkg")
;; (haskell-installed-packages-refresh-all)

;; Querying the data set:
;;
;; (mapcar (lambda (mod) (insert (concat mod "\n")))
;;         haskell-module-list-cache)
;;
;; (Try this in *scratch*.)

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

(defvar haskell-ghc-pkg-bin-path "ghc-pkg")
(defvar haskell-ghc-pkg-list-regexp "^{?[a-zA-Z0-9-_]+-[0-9.]+}?$")
(defvar haskell-ghc-pkg-list-cache nil)
(defvar haskell-module-list-cache nil)

(defun haskell-installed-packages-refresh-all ()
  "Refresh all caches in this module."
  (interactive)
  (message "Refreshing all installed Haskell packages cache...")
  (haskell-installed-packages t)
  (haskell-installed-modules t)
  (message "Done."))

(defun haskell-installed-packages (&optional refresh-cache)
  "Return a (cached) lisp list of all installed Haskell packages."
  (if (or refresh-cache (not haskell-ghc-pkg-list-cache))
      (setq haskell-ghc-pkg-list-cache (haskell-ghc-pkg-list-get))
    haskell-ghc-pkg-list-cache))

(defun haskell-ghc-pkg-command (arg)
  "Run a ghc-pkg command."
  (shell-command-to-string (concat haskell-ghc-pkg-bin-path " " arg)))

(defun haskell-ghc-pkg-list-get ()
  "Get the list of installed packages from ghc-pkg."
  (let* ((lines (split-string (haskell-ghc-pkg-command "list")
                              "\n    ")))
    (mapcar
     (lambda (line) 
       (string-match "^{?\\([a-zA-Z0-9-_]+-[0-9.]+\\)}?$" line)
       (match-string 1 line))
     (delete-if
      (lambda (line)
        (not (string-match haskell-ghc-pkg-list-regexp line)))
      lines))))

(defun haskell-installed-modules (&optional refresh-cache)
  "Get the list of installed modules from all installed packages."
  (if (and haskell-module-list-cache (not refresh-cache))
      haskell-module-list-cache
    (setq haskell-module-list-cache
          (haskell-installed-modules-get))))

(defun haskell-installed-modules-get ()
  "Get the list of installed modules from all installed packages."
  (apply 'append
         (mapcar (lambda (name)
                   (haskell-pkg-exposed-modules
                    (haskell-read-pkg-description
                     (haskell-ghc-pkg-command (concat "describe " name)))))
                 (haskell-installed-packages))))

(defun haskell-read-pkg-description (package)
  "Return an association list of key-values from a pkg description string."
  (let* ((marked (replace-regexp-in-string
                  "\n\\([^ ]\\)"
                  (lambda (match)
                    (concat "\n:" (substring match 1)))
                  package))
         (alist (mapcar 'haskell-pkg-key-value
                        (split-string marked "\n:"))))
    alist))

(defun haskell-pkg-key-value (entry)
  "Return a (key . value) pair from an entry."
  (let ((key-values (split-string entry ": ")))
    (if (listp key-values)
        (cons (car key-values)
              (replace-regexp-in-string
               "\n[ ]*"
               " "
               (mapconcat 'identity (cdr key-values) ": ")))
      key-values)))

(defun haskell-pkg-exposed-modules (pkg-alist)
  "Given a package alist, return a list of exposed modules."
  (let ((entry (assoc "exposed-modules" pkg-alist)))
    (when entry (split-string (cdr entry) " "))))

(provide 'haskell-installed-packages)