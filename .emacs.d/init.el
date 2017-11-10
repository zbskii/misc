;;; Package -- Brett Carter's Emacs init.el

;; Paths are set in variable customizations
;; (setenv "PATH"
;;   (concat
;;    "/usr/local/bin/" ":"
;;    "/usr/local/git/bin/" ":"
;;    "/Library/Frameworks/Python.framework/Versions/Current/bin/" ":"
;;    "/usr/local/go/bin/" ":"
;;    "${HOME}/bin" ":"
;;    "${HOME}/go/bin" ":"
;;    (getenv "PATH")
;;   )
;;   )

;; Package config
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Use unicode!
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(column-number-mode t)

;; Search path for executables
;; (setq exec-path (append exec-path
;;                         '("/usr/local/bin/")))
;; (setq exec-path (append exec-path
;;                         '("/usr/local/git/bin/")))
;; (setq exec-path (append exec-path
;;                         '("/usr/local/go/bin/")))
;; (setq exec-path (append exec-path
;;                         '("/Library/Frameworks/Python.framework/Versions/Current/bin/")))
;; (setq exec-path (append exec-path
;;                         '("~/bin/")))


(savehist-mode 1)

;; Zoom in and out globally instead of buffer by buffer
(defadvice text-scale-increase (around all-buffers (arg) activate)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      ad-do-it)))

;; Implement play-sound on OSX versions of Emacs
(add-to-list 'load-path "~/.emacs.d/play-sound")
(unless (and (fboundp 'play-sound-internal)
             (subrp (symbol-function 'play-sound-internal)))
  (require 'play-sound))

;; Disable scrolling bell
(defun my-bell-function ()
  (unless (memq this-command
            '(isearch-abort abort-recursive-edit exit-minibuffer
              keyboard-quit mwheel-scroll down up next-line previous-line scroll-down-command scroll-up-command
              backward-char forward-char end-of-buffer beginning-of-buffer keyboard-quit isearch-repeat-forward
              ))
    ;;(message "Command that cause the bell is %s" this-command)
    (play-sound-file "~/.emacs.d/sfx/smw_stomp.wav")))
(setq ring-bell-function 'my-bell-function)
;; fuck it, disable the bell.
(setq ring-bell-function 'ignore)

;; spaces, no tabs
(setq-default indent-tabs-mode nil)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

; automatically pair quotes and such
; Should really try https://github.com/Fuco1/smartparens
(electric-pair-mode t)
; highlight the current line
(global-hl-line-mode)
; delete selections when yanking etc
(delete-selection-mode t)

;; Highlight tabs and lines > 80 cols
(require 'whitespace)
(global-whitespace-mode 1)
(setq whitespace-global-modes (not "PHP/l")) ;; Disable ws mode for php
;;(setq whitespace-style (quote (tabs space-mark tab-mark newline-mark)))
(setq whitespace-style (quote ( face space-before-tab tabs tab-mark  lines-tail)))
(setq whitespace-display-mappings
 '(
   ;; These are lists of characters to replace, so newline-mark
   ;; replaces '10' with the sequence '182 10'
   (space-mark 32 [183] [46]) ; normal space
   (space-mark 160 [164] [95])
   (space-mark 2208 [2212] [95])
   (space-mark 2336 [2340] [95])
   (space-mark 3616 [3620] [95])
   (space-mark 3872 [3876] [95])
   (newline-mark 10 [182 10]) ; newlne
   (tab-mark 9 [8677 9] [92 9]) ; tab
))

;;; automatically check spelling
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Dired extra commands
(autoload 'dired-jump "dired-x"
  "Jump to dired buffer corresponding to current buffer."
  'interactive)
(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (`dired-jump') but in other window."
  'interactive)
(setq dired-bind-jump t)
(global-set-key "\C-x\C-j" 'dired-jump)
(global-set-key "\C-x4\C-j" 'dired-jump-other-window)


;; Fix scrolling
(setq
  scroll-margin 0
  scroll-conservatively 100000
  scroll-preserve-screen-position 1)

;; Fix mousewheel scrolling
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))

(desktop-save-mode 1)

(setq-default show-trailing-whitespace t)

;; Packages

(use-package better-defaults
  :ensure t)

(use-package twilight-anti-bright-theme
  :ensure t
  :config
  ;;(load-theme 'twilight-anti-bright)
 )

;; (use-package twilight-theme
;;   :ensure t
;;   :config
;;   (load-theme 'twilight))


;; (use-package color-theme
;;   :ensure t
;;   :config
;;   (color-theme-initialize)
;;   (load-file "~/.emacs.d/themes/color-theme-twilight.el")
;;   (color-theme-twilight))

;; auto-complete mode
(use-package auto-complete
  :ensure t
  :config
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (require 'auto-complete-config)
  (ac-config-default)
  (global-auto-complete-mode t)
  (setq ac-use-quick-help t)
  (set-face-background 'ac-candidate-face "grey20")
  (set-face-underline 'ac-candidate-face "#111111")
  (set-face-background 'ac-selection-face "#141414"))

(use-package haskell-mode
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-color-mode-line
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(use-package flycheck-pos-tip
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(use-package flycheck-haskell
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)))

(use-package helm-flyspell
  :ensure t)

(use-package flyspell-lazy
  :ensure t
  :config
  (define-key flyspell-mode-map (kbd "C-;") 'helm-flyspell-correct))

;; Fix incompatibility with flyspell
(ac-flyspell-workaround)

;; Completion words longer than 4 characters
(use-package ac-ispell
  :ensure t
  :config
  (custom-set-variables
   '(ac-ispell-requires 4)
   '(ac-ispell-fuzzy-limit 2))
  (eval-after-load "auto-complete"
  '(progn
      (ac-ispell-setup))))

(eval-after-load "auto-complete"
  '(progn
      (ac-ispell-setup)))

;; Completion words longer than 4 characters
(custom-set-variables
  '(ac-ispell-requires 4)
  '(ac-ispell-fuzzy-limit 2))

(eval-after-load "auto-complete"
  '(progn
      (ac-ispell-setup)))

(use-package php-mode
  :ensure t)

(defun my-php-mode-stuff ()
  (local-set-key (kbd "<f1>") 'my-php-function-lookup)
  (local-set-key (kbd "<s-f1>") 'my-php-symbol-lookup)
  (setq indent-tabs-mode t) ;; Waaaaa for appnexus
)

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
            (mbessage "Could not extract function info. Press C-F1 to go the description."))))
    (kill-buffer buf)))

(use-package go-autocomplete
  :ensure t)

;; golang
(use-package go-mode
  :ensure t
  :config
  (setq gofmt-command "goimports")               ; gofmt uses invokes goimports
  (go-guru-hl-identifier-mode)                   ; highlight identifiers
  (require 'go-autocomplete)
  (auto-complete-mode 1)                         ; Enable auto-complete mode
  (add-hook 'go-mode-hook (lambda ()
                            (local-set-key (kbd "C-c C-k") 'godoc-at-point)
                            (local-set-key (kbd "M-.") 'godef-jump)
                            (local-set-key (kbd "M-*") 'pop-tag-mark)
                            (local-set-key (kbd "M-p") 'compile)
                            (local-set-key (kbd "M-P") 'recompile)
                            (local-set-key (kbd "M-]") 'next-error)
                            (local-set-key (kbd "M-[") 'previous-error)))
)

;; go-guru
(use-package go-guru
  :ensure t)

;; magit
(use-package magit
  :ensure t)

;; Powerline
(use-package powerline
             :ensure t
             :config
             (powerline-default-theme))

(use-package sbt-mode
  :ensure t
  :config)

(use-package scala-mode2
  :ensure t
  :config
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

(use-package ensime
  :ensure t
  :config
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

;; ;; YASnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; PHP XDebugger
;; PUT THIS LAST.  Not really sure why but this breaks everything
;;(autoload 'geben)
;;(use-package geben
;;   :ensure t
;;   :config
;;   (setq geben-pause-at-entry-line nil))
(setq geben-pause-at-entry-line nil)

(use-package helm
  :ensure t
  :config
  (require 'helm-config)
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t)
  (helm-autoresize-mode t)
  (global-set-key (kbd "M-x")                          'undefined)
  (global-set-key (kbd "M-x")                          'helm-M-x)
  (global-set-key (kbd "C-x r b")                      'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f")                      'helm-find-files)
  (global-set-key (kbd "C-x b")                        'helm-mini)
  (global-set-key (kbd "M-y")                          'helm-show-kill-ring))

;; Show count of isearch results
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode t))

;; (use-package mo-git-blame
;;   :ensure t
;;   :config
;;   (autoload 'mo-git-blame-file "mo-git-blame" nil t)
;;   (autoload 'mo-git-blame-current "mo-git-blame" nil t)
;;   (global-set-key [?\C-c ?g ?c] 'mo-git-blame-current)
;;   (global-set-key [?\C-c ?g ?f] 'mo-git-blame-file))

(use-package shell-pop
  :ensure t)

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
(setq tramp-default-method "scpx") ;; fixes problems with ControlMaster

;; ;;(require 'grep-edit) - need to try grep-a-lot

;; Keybindings
(global-set-key (kbd "<C-tab>") 'bury-buffer)
(set-frame-height (selected-frame) 49)
(set-frame-width (selected-frame) 163)

(split-window-horizontally)

;; Put autosave files in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; load custom settings
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(provide 'init)
;;; init.el ends here
(put 'downcase-region 'disabled nil)
