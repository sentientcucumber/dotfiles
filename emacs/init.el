;;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Appearance

(when window-system
  (use-package dracula-theme
    :ensure t
    :init   (load-theme 'dracula t))
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (blink-cursor-mode -1)
  (column-number-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Settings

(setq-default indent-tabs-mode nil)

(setq inhibit-startup-message t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(setq vc-follow-symlinks t)

;; Settings for managing the backup files that Emacs automatically
;; creates. Found this helpful
;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(setq backup-directory-alist `(("." . "~/.emacs.d/backups"))
      delete-old-versions t
      kept-new-versions 1
      version-control t)

(defalias 'yes-or-no-p 'y-or-n-p
  "Use 'y' and 'n' to answer yes or no questions.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Evil 

(use-package evil
  :ensure   t
  :diminish undo-tree-mode
  :init     (evil-mode t)
  :bind     (:map evil-normal-state-map
                  ("s" . evil-snipe-s)
                  ("S" . evil-snipe-S))
  :config
  (evil-snipe-mode t)
  ;; Reconfigure keys for the Dvorak layout to match QWERTY key
  ;; bindings.
  (dolist (key-map (list evil-normal-state-map
                         evil-visual-state-map
                         evil-motion-state-map))
    (bind-keys :map key-map
               ("d" . evil-backward-char)
               ("n" . evil-forward-char)
               ("h" . evil-next-visual-line)
               ("t" . evil-previous-visual-line)
               ("k" . evil-delete)
               ("K" . evil-delete-line)
               ("/" . swiper)))
  ;; Define default modes for given major modes.
  (add-to-list 'evil-emacs-state-modes 'custom-new-theme-mode)
  (setq evil-want-C-u-scroll t))

(use-package evil-escape
  :ensure   t
  :diminish evil-escape-mode
  :init     (evil-escape-mode t)
  :config
  ;; Unfortunately, because Dvorak's home row contains the most
  ;; commonly typed letters, most home row sequences are triggered
  ;; accidentally when typing.
  (setq evil-escape-key-sequence            "gc"
        evil-escape-unordered-key-sequence  t))

(use-package evil-lispy
  :disabled t                           ; Too much of a noob
  :ensure   t
  :diminish evil-lispy-mode
  :commands (evil-lispy-mode)
  :bind     (:map lispy-mode-map
                  ("h" . special-lispy-down)
                  ("t" . special-lispy-up)
                  ("d" . special-lispy-left)
                  ("n" . special-lispy-right)
                  ("k" . lispy-kill)))

(use-package evil-snipe
  :diminish evil-snipe-local-mode
  :ensure   t
  :commands (evil-snipe-s evil-snipe-S))

(use-package evil-surround
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; General features

(use-package ivy
  ;; Counsel depends on ivy and swiper.
  :ensure   counsel
  :diminish ivy-mode
  :bind     (:map ivy-minibuffer-map
                  ("C-h" . ivy-next-line)
                  ("C-t" . ivy-previous-line))
  :init     (ivy-mode t)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-fixed-height-minibuffer t
        ivy-count-format "%d/%d "
        ivy-height 10
        ivy-wrap t
        ivy-extra-directories nil))

(use-package counsel
  :ensure t)

(use-package nlinum
  :ensure t)

(use-package flyspell
  :ensure   t
  :if       (executable-find "hunspell")
  :diminish flyspell-mode
  :config   (setq ispell-program-name "hunspell"
                  flyspell-issue-message-flag nil
                  flyspell-issue-welcome-flag nil)
  :init
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (add-hook 'text-mode-hook #'flyspell-mode))

(use-package drag-stuff
  :ensure t
  :diminish drag-stuff-mode
  :init   (drag-stuff-global-mode t)
  :config (drag-stuff-define-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Programming modes

(add-hook
 'prog-mode-hook
 (lambda ()
   "Configuration for all modes inheriting from `prog-mode'."
   (show-paren-mode t)
   (nlinum-mode t)
   (smooth-scrolling-mode t)
   (turn-on-evil-surround-mode)))

(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   "Configuration for `emacs-lisp' mode."
   (evil-lispy-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Text modes

(add-hook
 'org-mode-hook
 (lambda ()
   "Configuration for `org-mode'."
   (set-fill-column 70)
   (turn-on-auto-fill)))

(use-package json-mode
  :ensure t
  :config (setq json-reformat:indent-width 2
                js-indent-level 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Hydras

(use-package hydra
  :ensure   t
  :commands (defhydra))
