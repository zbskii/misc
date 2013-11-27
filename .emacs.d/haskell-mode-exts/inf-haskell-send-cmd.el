;; Send commands to inferior Haskell without showing the command.
;; Copyright (C) 2010 Chris Done <chrisdone@gmail.com>

;; This module is intended for Haskell mode users, and requires
;; inf-haskell.el (inferior haskell mode).

;; Its purpose is to send commands to the inferior haskell
;; buffer, without displaying the command used and looking messy.

;; This module also provides a convenient interface to Cabal. It
;; works well with `inferior-haskell-find-project-root` set to
;; nil. The commands available are:
;;
;;  1. inferior-haskell-cabal
;;  2. inferior-haskell-ido-cabal-call

;; This module also provides a convenient interface to HLint. The
;; commands available are:
;;
;;  1. inferior-haskell-hlint
;;  2. inferior-haskell-hlint-file
;;
;; 1. Example usage:
;; 
;; (require 'inf-haskell-send-cmd)
;; (setq inferior-haskell-paths 
;;   (list "/home/chris/Programs/bin" "/home/chris/.cabal/bin"))
;; (define-key haskell-mode-map (kbd "C-c C-c")
;;     (lambda (interactive)
;;        (inferior-haskell-cabal "build")))
;; (define-key haskell-mode-map (kbd "C-c c")
;;             'inferior-haskell-ido-cabal-call)
;;
;; 2. Example usage:
;;
;; (require 'inf-haskell-send-cmd)
;; (setq inferior-haskell-paths 
;;   (list "/home/chris/Programs/bin" "/home/chris/.cabal/bin"))
;; (define-key haskell-mode-map (kbd "C-c l") 'inferior-haskell-hlint-file)
;;
;; or, possibly, try binding it with the load command:
;; 
;; (define-key haskell-mode-map [f5] 
;;   (lambda ()
;;     (interactive)
;;     (inferior-haskell-load-file)
;;     (inferior-haskell-hlint-file)))
;;
;; The nice thing about using hlint from inside inferior haskell
;; mode is that filenames are treated as they are in compile
;; errors, and thus are clickable.

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

(defvar inferior-haskell-paths nil
  "List of paths to include in the shell command.")
(defvar inferior-haskell-cabal-path "cabal"
  "Location of the cabal binary.")
(defvar inferior-haskell-hlint-path "hlint"
  "Location of the cabal binary.")

(defun inferior-haskell-send-command-ex (proc str &optional no-echo)
  "Run a command in the inferior haskell buffer, optionally echoing it."
  (setq str (concat str "\n"))
  (with-current-buffer (process-buffer proc)
    (inferior-haskell-wait-for-prompt proc)
    (goto-char (process-mark proc))
    (if no-echo
      (insert-before-markers "\n")
      (insert-before-markers str))
    (move-marker comint-last-input-end (point))
    (setq inferior-haskell-seen-prompt nil)
    (comint-send-string proc str)))

(defun inferior-haskell-send-cmd (cmd)
  "Send an arbitrary command to the inferior haskell buffer, without echoing."
  (interactive)
  (let ((buffer (current-buffer)))
    (switch-to-haskell)
    (inferior-haskell-send-command-ex (inferior-haskell-process) cmd t)
    (select-window (get-window-with-predicate
                    (lambda (w) (equal (window-buffer w) buffer))))))

(defun inferior-haskell-send-shell (args)
  "Send a shell command to the inferior haskell buffer."
  (interactive)
  (inferior-haskell-send-cmd
   (concat
    (if inferior-haskell-paths
        (concat ":!export PATH=$PATH:" 
                (mapconcat 'identity inferior-haskell-paths ":")
                ";")
      "")
     args)))

(defun inferior-haskell-ido-cabal-call ()
  "Send a cabal command and interactively choose which command to send."
  (interactive)
  (inferior-haskell-cabal
   (ido-completing-read
    "Cabal: "
    (let ((out (shell-command-to-string
                (concat
                 (if inferior-haskell-paths
                     (concat "export PATH=$PATH:" 
                             (mapconcat 'identity inferior-haskell-paths ":")
                             ";")
                   "")
                 inferior-haskell-cabal-path " --help"))))
      (remove-if (lambda (cmd)
                   (or (equal "cabal" cmd)
                       (not cmd)))
                 (mapcar (lambda (line)
                           (when (string-match "^  \\([a-z0-9]+\\)" line)
                             (match-string 1 line)))
                         (split-string out "\n")))))))

(defun inferior-haskell-cabal (args)
  "Run cabal with argument(s) in the inferior haskell buffer."
  (interactive)
  (inferior-haskell-send-shell
   (concat
    "cd "
    (inferior-haskell-find-project-root (current-buffer))
    " && "
    inferior-haskell-cabal-path
    " "
    args)))

(defun inferior-haskell-hlint-file ()
  (interactive)
  "Run hlint on the file in the current buffer."
  (inferior-haskell-hlint (buffer-file-name)))

(defun inferior-haskell-hlint (args)
  "Run hlint with argument(s) in the inferior haskell buffer."
  (interactive)
  (inferior-haskell-send-shell
   (concat
    inferior-haskell-hlint-path
    " "
    args)))

(provide 'inf-haskell-send-cmd)