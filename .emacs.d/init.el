;;; load custom settings
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Package config
(require 'package)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
;; For scala-mode2
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)


;; install use-package
(package-initialize)
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

;; spaces, no tabs
(setq-default indent-tabs-mode nil)

; automatically pair quotes and such
(electric-pair-mode t)
; highlight the current line
(global-hl-line-mode)
; delete selections when yanking etc
(delete-selection-mode t)

;; Highlight tabs and lines > 80 cols
(global-whitespace-mode t)
(setq whitespace-style (quote ( face space-before-tab lines-tail)))
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

(setq term-ansi-default-program "/bin/bash -l")

(desktop-save-mode 1)

(setq-default show-trailing-whitespace t)

;; auto-complete mode

(use-package auto-complete
    :ensure t
    :config
    (ac-config-default)
    (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
    (global-auto-complete-mode t)
    (setq ac-use-quick-help t)
    (set-face-background 'ac-candidate-face "grey20")
    (set-face-underline 'ac-candidate-face "#111111")
    (set-face-background 'ac-selection-face "#141414"))


;; save cursor position in open files
(require 'saveplace)
(setq-default save-place t)
;; Put autosave files in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


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

(use-package flyspell-lazy
  :ensure t)

(use-package php-mode
  :ensure t
  :config
  (add-hook 'php-mode-hook 'my-php-mode-stuff))

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

;; PHP XDebugger
(use-package geben
  :ensure t
  :config
  (setq geben-pause-at-entry-line nil)
  )

;; Color themes
(use-package color-theme
  :ensure t
  :config
  (load-file "~/.emacs.d/themes/color-theme-twilight.el")
  (load-file "~/.emacs.d/themes/zenburn.el")
  (color-theme-initialize)
  (color-theme-twilight))

;; YASnippet
;; (add-to-list 'load-path "~/.emacs.d/yasnippet-0.6.1c")
;; (require 'yasnippet) ;; not yasnippet-bundle
;; (yas/initialize)
;; (yas/load-directory "~/.emacs.d/yasnippet-0.6.1c/snippets")
;; Not for aquaemacs (setq visible-bell nil)

;;(require 'grep-edit) - need to try grep-a-lot


;; ido - interactively do things
(require 'ido)
(ido-mode 'both) ;; for buffers and files
(setq
  ido-save-directory-list-file "~/.emacs.d/cache/ido.last"

  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"

     "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
  ido-work-directory-list '("~/" "~/Desktop" "~/Documents")
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
(use-package magit
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

;; Powerline
(use-package powerline
    :config
    (powerline-default-theme))

;; Keybindings
(global-set-key (kbd "<C-tab>") 'bury-buffer)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#141414" :foreground "#F8F8F8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "apple" :family "Menlo"))))
 '(flymake-errline ((((class color) (background dark)) (:background "#332323" :foreground "#e37170"))))
 '(flymake-warnline ((((class color) (background dark)) (:background "#363636")))))

(set-frame-height (selected-frame) 49)
(set-frame-width (selected-frame) 163)

(split-window-horizontally)
