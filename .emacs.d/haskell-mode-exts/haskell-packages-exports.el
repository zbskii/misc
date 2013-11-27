;; THIS MODULE IS UNSTABLE/UNTESTED, PROBABLY WON'T WORK/
;;
;; Generate a database of all exports of all installed packages.
;; Copyright (C) 2010 Chris Done <chrisdone@gmail.com>

;; Usage:
;; (require 'haskell-package-exports)
;; (haskell-package-exports-refresh)

;; Running this function the first time will take a while to
;; extract and generate Hoogle .txt files for all the packages on
;; your system, but subsequent calls are cached and change-aware,
;; and thus miniscule.

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

(defvar haskell-packages-location "~/.cabal/packages")
(defvar haskell-ghc-pkg-bin-path "ghc-pkg")
(defvar haskell-ghc-pkg-list-regexp "^{?[a-zA-Z0-9-_]+-[0-9.]+}?$")
(defvar haskell-cabal-bin-paths "~/.cabal/bin")
(defvar haskell-exports-file "/home/chris/.cabal/exports.txt")

(defun haskell-packages-exports-refresh ()
  "Generate a database of all exports of all installed packages."
  )

(defun haskell-package-exports-hoogle-convert ()
  "Convert the hackage exports to a hoogle database."
  (shell-command-to-string 
   (concat "export PATH=$PATH:" haskell-cabal-bin-paths
           ";hoogle --convert=" haskell-exports-file)))

(defun haskell-package-exports ()
  "Generate an exports file for a package."
  (let* ((packages (haskell-installed-packages))
         (len (length packages))
         (i 1))
    (shell-command (concat "rm " haskell-exports-file))
    (mapc (lambda (p)
            (message (concat "(" (number-to-string i) " of "
                             (number-to-string len) ") "
                             "Generating database for " p " ..."))
            (let ((txt (haskell-package-generate-hoogle p)))
              (when txt
                (shell-command
                 (concat "cat " txt " >> " haskell-exports-file))))
            (setq i (+ i 1)))
          packages)
    (haskell-package-exports-hoogle-convert)))

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

(defun haskell-package-generate-hoogle (name-and-version &optional directory)
  "Extract a package, configure it and a Hoogle file."
  (let ((file (haskell-package-hoogle-file name-and-version directory)))
    (if (file-exists-p file)
        file
      (progn
        (and (haskell-package-extract-archive name-and-version directory)
             (haskell-package-cabal name-and-version "configure" directory))
        (haskell-package-run-hoogle name-and-version directory)))))

(defun haskell-package-directory (package-name-and-version)
  "Returns the directory of an installed package, version required
e.g. foo-1.2."
  (let* ((name-and-version (haskell-package-name-and-version
                            package-name-and-version))
         (name (car name-and-version))
         (version (cdr name-and-version))
         (repo (find-if
                (lambda (repo)
                  (let ((dir (concat haskell-packages-location
                                     "/" repo "/" name "/" version)))
                    (and (file-exists-p dir)
                         (file-exists-p (concat dir "/" 
                                                package-name-and-version
                                                ".tar.gz")))))
                (directory-files haskell-packages-location))))
    (when repo
      (concat haskell-packages-location
              "/" repo "/" name "/" version))))

(defun haskell-package-extract-archive (name-and-version &optional directory)
  "Extract the archive of a package or return t if already extracted."
  (let ((dir (or directory
                 (haskell-package-directory name-and-version)))
        (archive (concat name-and-version ".tar.gz")))
    (when dir
      (or (file-newer-than-file-p (concat dir "/" name-and-version)
                                  (concat dir "/" archive))
          (when (equal ""
                       (shell-command-to-string
                        (concat "cd " dir " && tar xf " archive)))
            (shell-command-to-string
             (concat "touch " (concat dir "/" name-and-version))))))))

(defun haskell-package-hoogle-file (name-and-version &optional directory)
  "Does a package's Hoogle file already exist?"
  (let* ((n-and-v (haskell-package-name-and-version
                   name-and-version))
         (dir (and n-and-v
                   (or directory
                       (haskell-package-directory name-and-version))))
         (name (and n-and-v (car n-and-v)))
         (docs (concat dir "/" name-and-version
                       "/dist/doc/html/" name "/" name ".txt")))
    docs))

(defun haskell-package-run-hoogle (name-and-version &optional directory)
  "Generate a Hoogle plain text file for a package."
  (let* ((dir (or directory
                  (haskell-package-directory name-and-version)))
         (file (haskell-package-hoogle-file name-and-version dir)))
    (when dir
      (if (file-exists-p file)
          file
        (let ((output (shell-command-to-string
                       (concat "export PATH=$PATH:" haskell-cabal-bin-paths
                               "; cd " (concat dir "/" name-and-version)
                               " && cabal haddock --hoogle"))))
          (when (string-match "Documentation created: \\(.+\\.txt\\)$"
                              output)
            (concat dir "/" name-and-version "/"
                    (match-string 1 output))))))))

(defun haskell-package-cabal (name-and-version args &optional directory)
  "Run a cabal command within a package's directory, return t on success."
  (let ((dir (or directory
                 (haskell-package-directory name-and-version))))
    (when (and dir (file-exists-p (concat dir "/" name-and-version)))
      (= 0 (shell-command
            (concat "export PATH=$PATH:" haskell-cabal-bin-paths
                    "; cd " (concat dir "/" name-and-version)
                    " && cabal " args))))))

(defun haskell-package-name-and-version (package-name-and-version)
  "Return a pair of the name and the version."
  (string-match "^\\(.+\\)-\\([0-9.]+\\)$" package-name-and-version)
  (cons (match-string 1 package-name-and-version)
        (match-string 2 package-name-and-version)))

(provide 'haskell-packages-exports)