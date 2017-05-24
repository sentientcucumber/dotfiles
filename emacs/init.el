;;; -*- lexical-binding: t; -*-

;; Standard package setup and add the MELPA repository.

;; TODO Investigate Quelpa sometime. Good way to manage Emacs packages
;; that aren't in MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(require 'use-package)

;; Default settings that apply across all modes on all buffers
(setq-default indent-tabs-mode nil)

;; Startup settings
(setq inhibit-startup-message  t
      initial-major-mode       'fundamental-mode
      initial-scratch-message  nil)

;; View settings
(when window-system
  (scroll-bar-mode     -1)
  (tool-bar-mode       -1)
  (menu-bar-mode       -1)
  (blink-cursor-mode   -1)
  (column-number-mode  1))

(load-theme 'geek t)

;; File settings
(setq vc-follow-symlinks t)

;; Settings for managing the backup files that Emacs automatically
;; creates. Found this helpful
;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(setq backup-directory-alist  `(("." . "~/.emacs.d/backups"))
      delete-old-versions     t
      kept-new-versions       1
      version-control         t)

(defalias 'yes-or-no-p 'y-or-n-p
  "Use 'y' and 'n' to answer yes or no questions.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil 

(use-package evil
  :ensure    t
  :diminish  undo-tree-mode
  :init      (evil-mode t)
  :config
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
               ("K" . evil-delete-line)))
  ;; Define default modes for given major modes.
  (add-to-list 'evil-emacs-state-modes 'custom-new-theme-mode))

(use-package evil-escape
  :ensure    t
  :diminish  evil-escape-mode
  :init      (evil-escape-mode t)
  :config
  ;; Unfortunately, because Dvorak's home row contains the most
  ;; commonly typed letters, most home row sequences are triggered
  ;; accidentally when typing.
  (setq evil-escape-key-sequence            "gc"
        evil-escape-unordered-key-sequence  t))

(use-package evil-lispy
  :ensure  t
  :bind    (:map evil-lispy-state-mode-map
                 ("h" . special-lispy-down)
                 ("n" . special-lispy-right)
                 ("t" . special-lispy-up)
                 ("d" . special-lispy-left)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General features

(use-package ivy
  :ensure    t
  :diminish  ivy-mode
  :bind      (:map ivy-minibuffer-map
                   ("C-h" . ivy-next-line)
                   ("C-t" . ivy-previous-line))
  :init      (ivy-mode t)
  :config
  (setq ivy-use-virtual-buffers      t
        ivy-fixed-height-minibuffer  t
        ivy-count-format             "%d/%d "
        ivy-height                   10
        ivy-wrap                     t
        ivy-extra-directories        nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Progamming modes

(add-hook
 'prog-mode-hook
 (lambda ()
   "Configuration for all modes inheriting from `prog-mode'."
   (show-paren-mode t)))

(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   "Configuration for `emacs-lisp' mode."
   (setq lisp-indent-function 'common-lisp-indent-function)
   (electric-pair-mode t)))
