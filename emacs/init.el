;;; init.el --- A masterpiece in progress

(require 'package)
(package-initialize)

;; theme
(load-theme 'smyx t)

;; dvorak ftw!
(define-key key-translation-map "\C-t" "\C-x")

;; global key bindings
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-x C-l") 'toggle-truncate-lines)
(global-set-key (kbd "M-'") 'other-window)

;; general
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; window settings
(when (eq window-system 'x)
  ;; Font
  (set-fontset-font "fontset-default" 'symbol "Fantasque Sans Mono")
  (set-default-font "Fantasque Sans Mono")
  (set-face-attribute 'default nil :height 100)

  ;; Remove all of the ugly bars
  (when (functionp 'menu-bar-mode)
    (menu-bar-mode -1))
  (when (functionp 'set-scroll-bar-mode)
    (set-scroll-bar-mode 'nil))
  (when (functionp 'mouse-wheel-mode)
    (mouse-wheel-mode -1))
  (when (functionp 'tooltip-mode)
    (tooltip-mode -1))
  (when (functionp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (functionp 'blink-cursor-mode)
    (blink-cursor-mode -1)))

(defalias 'yes-or-no-p 'y-or-n-p)

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(load-file (expand-file-name "settings.el" user-emacs-directory))
(load-file (expand-file-name "functions.el" user-emacs-directory))
(load-file (expand-file-name "packages.el" user-emacs-directory))
