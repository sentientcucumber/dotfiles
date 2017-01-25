;; -*- coding: utf-8; lexical-binding: t -*-

(package-initialize)
(require 'cask (concat
                (getenv "HOME")
                (if (eq window-system 'w32)
                    "\\.cask\\cask.el"
                  "/.cask/cask.el")))
(cask-initialize)

(setq-default indent-tabs-mode nil
              tab-always-indent t
              tab-width 4
              vc-follow-symlinks t
              mode-line-default-help-echo nil
              use-file-dialog nil)

(setq auto-save-default nil
      make-backup-files nil
      inhibit-startup-screen t
      initial-major-mode 'fundamental-mode)

(defalias 'yes-or-no-p 'y-or-n-p
  "Use 'y' and 'n' to answer yes/no questions, like a sane person.")

(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (blink-cursor-mode -1)
  (column-number-mode 1))

(use-package color-theme-sanityinc-tomorrow
  :init (load-theme 'sanityinc-tomorrow-night t))

(use-package smooth-scrolling
  :init (smooth-scrolling-mode t))

(use-package shackle
  :init
  (setq shackle-rules '(("*Help*" :select t :size 20 :align 'below)
                        ("*Warnings*" :select t :size 20 :align 'below)))
  (shackle-mode t))

(use-package idle-highlight-mode
  :init (setq idle-highlight-idle-time 0.75))

;;
;; `ivy'
;; 

(use-package ivy :demand
  :bind (:map ivy-minibuffer-map
              ("C-h" . ivy-next-line)
              ("C-t" . ivy-previous-line))
  :init (setq ivy-height 20
              ivy-wrap t)
  :config (ivy-mode t))

;;
;; `evil'
;;

(use-package evil
  :bind
  (:map
   evil-inner-text-objects-map
   ("a" . evil-inner-arg)
   :map
   evil-outer-text-objects-map
   ("a" . evil-outer-arg)
   :map
   evil-normal-state-map
   ("d" . evil-backward-char)
   ("n" . evil-forward-char)
   ("h" . evil-next-visual-line)
   ("t" . evil-previous-visual-line)
   ("j" . evil-snipe-X)
   ("k" . evil-delete)
   ("K" . evil-delete-line)
   ("l" . evil-search-next)
   ("L" . evil-search-previous)
   ("D" . evil-backward-arg)
   ("N" . evil-forward-arg)
   ("U" . evil-jump-out-args)
   :map evil-visual-state-map
   ("d" . evil-backward-char)
   ("n" . evil-forward-char)
   ("h" . evil-next-visual-line)
   ("t" . evil-previous-visual-line)
   ("j" . evil-find-char-to)
   ("k" . evil-delete)
   ("K" . evil-delete-line)
   :map evil-motion-state-map
   ("d" . evil-backward-char)
   ("n" . evil-forward-char)
   ("h" . evil-next-visual-line)
   ("t" . evil-previous-visual-line)
   ("j" . evil-find-char-to)
   ("k" . evil-delete)
   ("K" . evil-delete-line))
  :init
  (setq evil-echo-state nil
        evil-want-C-u-scroll t
        evil-find-skip-newlines t
        evil-normal-state-tag "N"
        evil-insert-state-tag "I"
        evil-visual-state-tag "V"
        evil-emacs-state-tag "E"
        evil-operator-state-tag "O"
        evil-motion-state-tag "M"
        evil-replace-state-tag "R"
        evil-default-cursor (face-attribute 'cursor
                                            :background nil
                                            'default)
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor `(,(face-attribute 'shadow
                                                    :background nil
                                                    'default)
                                   box)
        evil-operator-state-cursor '(hbar . 2)
        evil-emacs-state-cursor `(,(face-attribute 'font-lock-builtin-face
                                                   :foreground nil
                                                   'default)
                                  bar))
  (evil-mode 1))

(use-package evil-args
  :commands (evil-forward-arg evil-backward-arg))

(use-package evil-multiedit :demand 
  :commands
  (evil-multiedit-match-all
   evil-multiedit-match-and-next
   evil-multiedit-match-and-prev
   evil-multiedit-match-symbol-and-next
   evil-multiedit-match-symbol-and-prev
   evil-multiedit-toggle-or-restrict-region
   evil-multiedit-next
   evil-multiedit-prev
   evil-multiedit-abort
   evil-multiedit-ex-match)
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-easymotion
  :config
  (setq evilem-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)) ; Dvorak nonsense
  (evilem-define (kbd "g SPC h") 'evil-next-visual-line)
  (evilem-define (kbd "g SPC t") 'evil-previous-visual-line))

(use-package evil-snipe
  :config
  (setq evil-snipe-smart-case t
        evil-snipe-repeat-scope 'visible)
  :init
  (evil-snipe-mode t)
  (evil-snipe-override-mode t))

(use-package evil-surround)

(use-package general
  :init
  (setq general-default-keymaps 'evil-normal-state-map
        general-default-prefix "SPC")
  :config
  (general-define-key "SPC" 'counsel-M-x
                      "k" 'counsel-descbinds
                      "g" 'counsel-git
                      "f" 'counsel-find-file))

(use-package company
  :bind (:map company-active-map
              ("C-h" . company-select-next)
              ("C-t" . company-select-previous))
  :init (setq company-show-numbers t
              company-idle-delay 0.1
              company-auto-complete-chars (quote (41 46))))

(use-package nlinum
  :config (set-face-italic 'linum t))

(add-hook
 'prog-mode-hook (lambda ()
                   "Setup for all modes that inherit from `prog-mode'."
                   (company-mode t)
                   (column-enforce-mode t)
                   (hl-line-mode t)
                   (nlinum-mode t)
                   (show-paren-mode t)
                   (flyspell-prog-mode)
                   (evil-surround-mode t)
                   (electric-pair-local-mode t)
                   (idle-highlight-mode t)))

(add-hook
 'emacs-lisp-mode-hook (lambda ()
                         "Setup for the `emacs-lisp' mode."
                         (eldoc-mode t)
                         (highlight-quoted-mode t)
                         (setq-local evil-args-delimiters '(" "))
                         (setq dash-enable-fontlock t)))

;;
;; `python'
;;

(use-package python
  :init
  (setq python-shell-interpreter "python3")
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook #'anaconda-eldoc-mode))

(use-package company-anaconda :after python
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package virtualenv
  :config
  (setq virtualenv-root "~/.venvs"))

(use-package hy)

;;
;; `clojure'
;;

(use-package clojure
  :config (setq clojure-indent-style :always-indent))

;;
;; `groovy'
;;

(use-package groovy
  :init (setq c-basic-offset 4))

;;
;; `org'
;;

(defun geek/org-mode-hook ()
  "Setup for `org-mode'."
  (flyspell-mode t))

(add-hook 'org-mode-hook #'geek/org-mode-hook)

(use-package org
  :config
  (setq org-hide-leading-stars t)
  (set-face-attribute 'org-document-title nil :height 1.0))
