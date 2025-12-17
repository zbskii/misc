;;; init.el --- Brett Carter's Emacs config for Emacs 30.1

;;; Commentary:
;; This config uses `straight.el` for package management
;; and loads useful defaults and modes.

;;; Code:

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package with straight.el integration
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Encoding
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; History and visuals
(column-number-mode t)
(savehist-mode 1)
(blink-cursor-mode -1)
(global-hl-line-mode 1)
(delete-selection-mode t)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)

;; Whitespace
(use-package whitespace
  :config
  (setq whitespace-global-modes (not 'php-mode))
  (setq whitespace-style '(face space-before-tab tabs tab-mark lines-tail))
  (setq whitespace-display-mappings
        '((space-mark 32 [183] [46])
          (space-mark 160 [164] [95])
          (newline-mark 10 [182 10])
          (tab-mark 9 [8677 9] [92 9])))
  (global-whitespace-mode 1))

;; Scroll settings
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; Sound and bell
(setq ring-bell-function 'ignore)

;; PATH from shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Better defaults
(use-package better-defaults)

;; Auto-complete (note: consider migrating to company-mode in future)
(use-package auto-complete
  :config
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (require 'auto-complete-config)
  (ac-config-default)
  (global-auto-complete-mode t)
  (setq ac-use-quick-help t))

;; Spell checking
(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package helm-flyspell :after flyspell)
(use-package flyspell-lazy
  :ensure t
  :config
  (add-hook 'flyspell-mode-hook
            (lambda ()
              (define-key flyspell-mode-map (kbd "C-;") 'helm-flyspell-correct))))

;; Flycheck
(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-phpcs-standard "~/AppNexus"))

(use-package flycheck-color-mode-line
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package flycheck-pos-tip
  :config
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(use-package flycheck-haskell
  :hook (flycheck-mode . flycheck-haskell-setup))

;; PHP
(use-package php-mode
  :config
  (defun my-php-mode-stuff ()
    (local-set-key (kbd "<f1>") 'my-php-function-lookup)
    (local-set-key (kbd "<s-f1>") 'my-php-symbol-lookup)
    (setq indent-tabs-mode t))
  (add-hook 'php-mode-hook 'my-php-mode-stuff))

;; Function lookups for PHP
(defun my-php-symbol-lookup ()
  (interactive)
  (let ((symbol (symbol-at-point)))
    (if symbol
        (browse-url (format "http://php.net/manual-lookup.php?pattern=%s" (symbol-name symbol)))
      (message "No symbol at point."))))

(defun my-php-function-lookup ()
  (interactive)
  (let* ((function (symbol-name (or (symbol-at-point)
                                    (error "No function at point."))))
         (buf (url-retrieve-synchronously
               (format "http://php.net/manual-lookup.php?pattern=%s" function))))
    (with-current-buffer buf
      (goto-char (point-min))
      (let (desc)
        (when (re-search-forward "<div class=\"methodsynopsis dc-description\">\\(\\(.\\|\n\\)*?\\)</div>" nil t)
          (setq desc
                (replace-regexp-in-string
                 " +" " "
                 (replace-regexp-in-string
                  "\n" ""
                  (replace-regexp-in-string "<.*?>" "" (match-string-no-properties 1))))))
        (if desc
            (message desc)
          (message "Could not extract function info. Press C-F1 to go the description.")))
      (kill-buffer buf))))

;; JavaScript
(use-package js2-mode
  :mode "\\.js\\'")

;; Git
(use-package magit)

;; Powerline
(use-package powerline
  :config (powerline-default-theme))

;; Yasnippet
(use-package yasnippet
  :config (yas-global-mode 1))

;; Helm
(use-package helm
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring))

;; Project Management
(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package helm-projectile
  :after (helm projectile)
  :config
  (helm-projectile-on)
  (setq projectile-completion-system 'helm
        projectile-switch-project-action 'helm-projectile))

;; Isearch
(use-package anzu
  :config (global-anzu-mode t))

;; Shell
(use-package shell-pop)

;; Buffers and navigation
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; Window management
(use-package ace-window
  :bind (("s-m" . ace-window)
         ("C-x o" . ace-window)))

;; Tramp
(require 'tramp)
(setq tramp-verbose 10
      tramp-default-method "scpx")

;; Startup layout
(set-frame-height (selected-frame) 49)
(set-frame-width (selected-frame) 163)
(split-window-horizontally)

;; Backups
(setq backup-directory-alist '(("." . "~/backups"))
      backup-by-copying t)

;; Theme
(use-package twilight-anti-bright-theme
  :config (load-theme 'twilight-anti-bright t))

;; Custom file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Byte-compile all dotfiles
(defun er-byte-compile-init-dir ()
  "Byte-compile all files in the user init directory."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(provide 'init)
;;; init.el ends here
