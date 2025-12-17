(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-fuzzy-limit 2)
 '(ac-ispell-requires 4)
 '(ac-use-menu-map t)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes '(twilight-anti-bright))
 '(custom-safe-themes
   '("8aeb4dbed3dd5c639adcc574eb2e5698b08545dd3d1794ed7e9b4f2b8eb289e4" "dc8ad8b5833ae06e373cc3d64be28e67e6c3d084ea5f0e9e77225b3badbec661" "72085337718a3a9b4a7d8857079aa1144ea42d07a4a7696f86627e46ac52f50b" "95db78d85e3c0e735da28af774dfa59308db832f84b8a2287586f5b4f21a7a5b" "51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default))
 '(desktop-restore-eager 5)
 '(ediff-diff-options "-w")
 '(flycheck-php-phpcs-executable nil)
 '(flycheck-php-phpmd-executable nil)
 '(flycheck-phpcs-standard "~/AppNexus")
 '(flycheck-phpmd-rulesets '("design" "unusedcode"))
 '(package-enable-at-startup nil)
 '(package-selected-packages
   '(smartparens helm-projectile flymake-eslint exec-path-from-shell json-mode flycheck-gometalinter go-autocomplete go-guru zenburn-theme use-package twilight-theme twilight-anti-bright-theme solarized-theme shell-pop scala-mode2 powerline php-mode mo-git-blame magit js2-mode helm-flyspell go-mode git-timemachine geben flyspell-lazy flycheck-pos-tip flycheck-haskell flycheck-color-mode-line ensime csv-mode color-theme-solarized better-defaults anzu ac-ispell))
 '(powerline-default-separator 'utf-8)
 '(scroll-bar-mode nil)
 '(shell-pop-full-span t)
 '(shell-pop-shell-type
   '("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell))))
 '(shell-pop-term-shell "/bin/bash")
 '(shell-pop-universal-key "C-t")
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#141414" :foreground "#F8F8F8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "nil" :family "Cascadia Code"))))
 '(flycheck-error ((t (:background "#332323" :foreground "#e37170"))))
 '(flycheck-error-list-warning ((t (:inherit warning :background "#363636"))))
 '(flymake-error ((((class color) (background dark)) (:background "#332323" :foreground "#e37170"))))
 '(flymake-warning ((((class color) (background dark)) (:background "#363636"))))
 '(helm-candidate-number ((t (:background "grey20" :foreground "#8F9D6A"))))
 '(helm-grep-file ((t (:foreground "#e37170" :underline "#e37170"))))
 '(helm-header-line-left-margin ((t (:background "grey20" :foreground "purple"))))
 '(helm-match ((t (:background "#e37170" :foreground "#332323"))))
 '(helm-selection ((t (:background "grey20" :distant-foreground "purple")))))


