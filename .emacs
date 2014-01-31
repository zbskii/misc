(add-to-list 'load-path "~/.emacs.d")
(setq debug-on-error t)
;;(setq mac-allow-anti-aliasing nil)  ;; turn off anti-aliasing
(set-frame-height (selected-frame) 49)
(set-frame-width (selected-frame) 163)
(split-window-horizontally)

;; Disable scrolling bell
(defun my-bell-function ()
  (unless (memq this-command
            '(isearch-abort abort-recursive-edit exit-minibuffer
              keyboard-quit mwheel-scroll down up next-line previous-line
              backward-char forward-char beginning-of-buffer
              ))
    (ding)))
(setq ring-bell-function 'my-bell-function)

;; spaces, no tabs
(setq-default indent-tabs-mode nil)

;; Highlight tabs and lines > 80 cols
(global-whitespace-mode t)
(setq whitespace-style (quote ( tab-mark lines-tail )))
(setq whitespace-display-mappings
 '(
   (space-mark 32 [183] [46]) ; normal space
   (space-mark 160 [164] [95])
   (space-mark 2208 [2212] [95])
   (space-mark 2336 [2340] [95])
   (space-mark 3616 [3620] [95])
   (space-mark 3872 [3876] [95])
   (newline-mark 10 [182 10]) ; newlne
   (tab-mark 9 [8677 9] [92 9]) ; tab
))

(column-number-mode t)

;; Fix scrolling
(setq
  scroll-margin 0
  scroll-conservatively 100000
  scroll-preserve-screen-position 1)

;; Fix mousewheel scrolling
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))

(setq term-ansi-default-program "/bin/bash -l")

(desktop-save-mode 1)

(setq-default show-trailing-whitespace t)

;; auto-complete mode
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(global-auto-complete-mode t)
(setq ac-use-quick-help t)
(set-face-background 'ac-candidate-face "grey20")
(set-face-underline 'ac-candidate-face "#111111")
(set-face-background 'ac-selection-face "#141414")

;; Search path for executables
(setq exec-path (append exec-path
           '("/usr/local/bin/")))
(setq exec-path (append exec-path
           '("/usr/local/git/bin/")))
(setq exec-path (append exec-path
           '("/Library/Frameworks/Python.framework/Versions/Current/bin/")))
(setq exec-path (append exec-path
           '("~/bin")))


(setenv "PATH"
  (concat
   "/usr/local/bin/" ":"
   "/usr/local/git/bin/" ":"
   "/Library/Frameworks/Python.framework/Versions/Current/bin/" ":"
   "~/bin" ":"
   (getenv "PATH")
  )
)
(savehist-mode 1)

;; save cursor position in open files
(require 'saveplace)
(setq-default save-place t)

;; Put autosave files in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'package)
;; Add marmalade repo
;; Add the original Emacs Lisp Package Archive
;; (add-to-list 'package-archives
;;              '("elpa" . "http://tromey.com/elpa/"))
;; Add the user-contributed repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
;; For scala-mode2
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defun package-update-load-path ()
  "Update the load path for newly installed packages."
  (interactive)
  (let ((package-dir (expand-file-name package-user-dir)))
    (mapc (lambda (pkg)
            (let ((stem (symbol-name (car pkg)))
                  (version "")
                  (first t)
                  path)
              (mapc (lambda (num)
                      (if first
                          (setq first nil)
                          (setq version (format "%s." version)))
                      (setq version (format "%s%s" version num)))
                    (aref (cdr pkg) 0))
              (setq path (format "%s/%s-%s" package-dir stem version))
              (add-to-list 'load-path path)))
          package-alist)))


;; Flymake on-the-fly syntax checking
;; Use illusori's flymkae for to fix tramp issues
(add-to-list 'load-path "~/.emacs.d/emacs-flymake")
(when (load "flymake" t)
  ;; Nope, I want my copies in the system temp dir.
  (setq flymake-run-in-place nil)
  ;; This lets me say where my temp dir is.
  (setq temporary-file-directory "~/.emacs.d/tmp/")
  (setq flymake-log-level 0)

  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-copy))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "~/lib/pycheckers.py" (list local-file))))
  ;; *Only* allow python - flymake ships with a bunch of shit turned on
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init))
 )

(defun flymake-gjslint-init ()
  "Initialize flymake for gjslint"
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-copy)))
    (list "gjslint" (list temp-file "--nosummary"))))
(add-to-list 'flymake-allowed-file-name-masks
             '(".+\\.js$"
               flymake-gjslint-init
               flymake-simple-cleanup
               flymake-get-real-file-name))
(push '("^Line \\([[:digit:]]+\\), E:[[:digit:]]+: " nil 1 nil)
      flymake-err-line-patterns)
(add-hook 'js2-mode-hook (lambda () (flymake-mode t)))

(eval-after-load 'ruby-mode
  '(progn
     ;; Invoke ruby with '-c' to get syntax checking
     (defun flymake-ruby-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         ;; Ugh such a hack
         (list "/Users/brett/.rvm/rubies/ruby-1.9.3-p194/bin/ruby" (list "-wc" local-file))))

     (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
     (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

     (push '("^\\(^[a-zA-Z0-9_]\.rb\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3)
           flymake-err-line-patterns)

     (add-hook 'ruby-mode-hook
               (lambda ()
                 (when (and buffer-file-name
                            (file-writable-p
                             (file-name-directory buffer-file-name))
                            (file-writable-p buffer-file-name))
                   (local-set-key (kbd "C-c d")
                                  'flymake-display-err-menu-for-current-line)
                   (flymake-mode t))))))


;; Start puppet-mode when editing a .pp file
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
(require 'flymake-puppet)
(add-hook 'puppet-mode-hook (lambda () (flymake-puppet-load)))
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; haskell-mode
(add-to-list 'load-path "~/.emacs.d/haskell-mode")
(autoload 'haskell-mode "haskell-site-file" "mode for haskell" t)
(add-to-list 'auto-mode-alist '("\\.hs\\|.lhs" . haskell-mode))
(load "haskell-site-file" nil t)
;; highlight "return" as a builtin
(font-lock-add-keywords 'haskell-mode
 '(("\\<\\(return\\)\\>" 1 font-lock-builtin-face prepend)))
(setq haskell-indent-offset 4) ;; janrain default
;; haskell-mode-exts
(add-to-list 'load-path "~/.emacs.d/haskell-mode-exts")
(require 'haskell-align-imports)
(add-hook 'haskell-mode-hook 'turn-on-font-lock)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-c h") 'haskell-hoogle)))

;; Ruby
(eval-after-load 'ruby-mode
  '(progn
     ;; work around possible elpa bug
     (ignore-errors (require 'ruby-compilation))
     (setq ruby-use-encoding-map nil)
     ;;(add-hook 'ruby-mode-hook 'inf-ruby-keys)
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map (kbd "C-M-h") 'backward-kill-word)
     (define-key ruby-mode-map (kbd "C-c l") "lambda")))
;; ri mode
(setq ri-ruby-program "/usr/bin/ruby")
(setq ri-ruby-script "~/emacs.d/ri-emacs/ri-emacs.rb")
(autoload 'ri "~/.emacs.d/ri-emacs/ri-ruby.el" nil t)
(global-set-key (kbd "C-h r") 'ri)
;; Rakefile are ruby
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))

;; golang mode
(eval-after-load 'go-mode
  '(progn
     (setq-default indent-tabs-mode nil)
     (setq-default tab-width 4)
     ))

;; PHP
(require 'php-mode) ;; Custom php mode
(add-hook 'php-mode-hook 'my-php-mode-stuff)

(defun my-php-mode-stuff ()
  (local-set-key (kbd "<f1>") 'my-php-function-lookup)
  (local-set-key (kbd "<s-f1>") 'my-php-symbol-lookup))


(defun my-php-symbol-lookup ()
  (interactive)
  (let ((symbol (symbol-at-point)))
    (if (not symbol)
        (message "No symbol at point.")

      (browse-url (concat "http://php.net/manual-lookup.php?pattern="
                          (symbol-name symbol))))))

(defun my-php-function-lookup ()
  (interactive)
  (let* ((function (symbol-name (or (symbol-at-point)
                                    (error "No function at point."))))
         (buf (url-retrieve-synchronously (concat "http://php.net/manual-lookup.php?pattern=" function))))
    (with-current-buffer buf
      (goto-char (point-min))
        (let (desc)
          (when (re-search-forward "<div class=\"methodsynopsis dc-description\">\\(\\(.\\|\n\\)*?\\)</div>" nil t)
            (setq desc
              (replace-regexp-in-string
                " +" " "
                (replace-regexp-in-string
                  "\n" ""
                  (replace-regexp-in-string "<.*?>" "" (match-string-no-properties 1)))))

            (when (re-search-forward "<p class=\"para rdfs-comment\">\\(\\(.\\|\n\\)*?\\)</p>" nil t)
              (setq desc
                    (concat desc "\n\n"
                            (replace-regexp-in-string
                             " +" " "
                             (replace-regexp-in-string
                              "\n" ""
                              (replace-regexp-in-string "<.*?>" "" (match-string-no-properties 1))))))))

          (if desc
              (message desc)
            (message "Could not extract function info. Press C-F1 to go the description."))))
    (kill-buffer buf)))

;; Org mode
(setq org-startup-indented t)

;; Puppet mode
(add-to-list 'load-path "~/.emacs.d/puppet-syntax-emacs")
(autoload 'puppet-mode "puppet-mode-init" "mode for puppet" t)

;; Color themes
(add-to-list 'load-path "~/.emacs.d/color-theme")
(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/themes/color-theme-twilight.el")
(load-file "~/.emacs.d/themes/zenburn.el")
(color-theme-twilight)

;; YASnippet
;; (add-to-list 'load-path "~/.emacs.d/yasnippet-0.6.1c")
;; (require 'yasnippet) ;; not yasnippet-bundle
;; (yas/initialize)
;; (yas/load-directory "~/.emacs.d/yasnippet-0.6.1c/snippets")
;; Not for aquaemacs (setq visible-bell nil)

(add-to-list 'load-path "~/.emacs.d/")
(require 'grep-edit)


;; ido - interactively do things
(require 'ido)
(ido-mode 'both) ;; for buffers and files
(setq
  ido-save-directory-list-file "~/.emacs.d/cache/ido.last"

  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"

     "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
  ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~/src/janrain")
  ido-case-fold  t                 ; be case-insensitive

  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-directory-list 30   ; should be enough
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point nil    ; don't use filename at point (annoying)
  ido-use-url-at-point nil         ; don't use url at point (annoying)

  ido-enable-flex-matching t     ; don't try to be too smart
  ido-max-prospects 8              ; don't spam my minibuffer
  ido-confirm-unique-completion nil) ; wait for RET, even with unique completion
;; when using ido, the confirmation is rather annoying...
(setq confirm-nonexistent-file-or-buffer nil)

;; magit
(add-to-list 'load-path "~/.emacs.d/magit")
(autoload 'magit-status "magit" "magit" t)

;; git-blame mode
(add-to-list 'load-path "~/.emacs.d/mo-git-blame")
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)
(global-set-key [?\C-c ?g ?c] 'mo-git-blame-current)
(global-set-key [?\C-c ?g ?f] 'mo-git-blame-file)

;; fringe bitmaps
(setq default-indicate-empty-lines t)
(setq default-indicate-buffer-boundaries (quote left))

;; Better buffer naming than <1>, <2>, <3>
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; Cleanup old buffers
(require 'midnight)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(require 'tramp)
(setq tramp-debug-buffer t)
(setq tramp-verbose 10)

;; Keybindings
(global-set-key (kbd "<C-tab>") 'bury-buffer)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(desktop-restore-eager 5)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#141414" :foreground "#F8F8F8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "apple" :family "Menlo"))))
 '(flymake-errline ((((class color) (background dark)) (:background "#332323" :foreground "#e37170"))))
 '(flymake-warnline ((((class color) (background dark)) (:background "#363636")))))

(put 'narrow-to-page 'disabled nil)
