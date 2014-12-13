;;; init.el -- Summary
;;; Commentary:

;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(setq package-enable-at-startup nil)

(defconst f '(-1))

;; Default emacs settings
(setq inhibit-startup-message    t)
(setq ring-bell-function         (lambda()))
(initial-major-mode              'fundamental-mode)
(put 'erase-buffer               'disabled nil)

;; File reading/writing settings
(setq read-file-name-completion-ignore-case t)
(setq delete-auto-save-files                t)
(setq find-file-visit-truename              nil)

;; Appearances
(load-theme 'monokai             t)
(global-font-lock-mode           t)
(line-number-mode                t)
(column-number-mode              t)
(transient-mark-mode             t)
(menu-bar-mode                   f)
(tool-bar-mode                   f)
(blink-cursor-mode               f)

(setq vc-follow-symlinks         t)
(setq make-pointer-invisible     t)
(setq fill-column                80)
(setq echo-keystrokes            0.1)

;; Key Bindings
(global-set-key (kbd "C-s")      'isearch-forward-regexp)
(global-set-key (kbd "C-r")      'isearch-backward-regexp)
(global-set-key (kbd "M-%")      'query-replace-regexp)
(global-set-key (kbd "C-x C-l")  'toggle-truncate-lines)
(global-set-key (kbd "C-x +")    'balance-windows-area)
(global-set-key (kbd "RET")      'newline-and-indent)
(global-set-key (kbd "C-t")      'Control-X-prefix)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Custom variable
(custom-set-variables
 ;; XML is terrible, lets make reading/writing it a little better
 '(nxml-child-indent 4)
 '(nxml-slash-auto-complete-flag t))

;; this nifty bit lets the emacs kill ring and mac's buffer join in harmony
(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen nil)
  (setq insert-directory-program "gls")
  (setq dired-listing-switches "-aBhl --group-directories-first")
  (defun copy-from-osx ()
    (shell-command-to-string "/usr/bin/pbpaste"))
  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "/usr/bin/pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  (setq interprogram-cut-function 'paste-to-osx
        interprogram-paste-function 'copy-from-osx))

;; this fixed issues when opening directories on initial startup
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)

;; stop putting temp files in the same directory, if I need it put it elsewhere
;; (setq backup-directory-alist
;;       `((".*" . ,temporary-file-directory)))
;; (setq auto-save-file-name-transforms
;;       `((".*" ,temporary-file-directory t)))

;; C
(defun my/c-mode-init()
  (c-set-style             "k&r")
  (c-toggle-electric-state f)
  (hs-minor-mode           t)
  (setq c-basic-offset     4))
(add-hook 'c++-mode-hook #'my/c-mode-init)

;; Go
(defun my/go-mode-init()
  (setq c-basic-offset     8
        indent-tabs-mode   t))
(add-hook 'go-mode-hook #'my/go-mode-init)

;; XML
(defun my/nxml-mode-init()
  (setq subword-mode       t))
(add-hook 'nxml-mode-hook #'my/nxml-mode-init)

;; Find important comment words
(defun my/add-watchwords()
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\)\\>"
          1 '((:foreground "#d7a3ad") (:weight bold)) t))))

;; Remove tabs from the entire buffer
(defun untabify-buffer()
  (interactive)
  (untabify (point-min) (point-max)))

;; Indents entire buffer, be careful doing this on large files
(defun indent-buffer()
  (interactive)
  (indent-region (point-min) (point-max)))

;; General settings that should be applied to all programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (visual-line-mode t)
            (subword-mode t)
            (my/add-watchwords)))

;; package setup!
(require 'use-package)

;; setup uniquify package
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; setup smooth-scrollin package
(use-package smooth-scrolling
  :config (setq smooth-scroll-margin 8))

;; setup undo-tree
(use-package undo-tree
  :init (global-undo-tree-mode)
  :config
  (progn
    (define-key undo-tree-map (kbd "C-x u") 'undo-tree-visualize)
    (define-key undo-tree-map (kbd "C-u") 'undo-tree-undo)))

;; setup magit
(use-package magit
  :bind ("M-g M-g" . magit-status))

;; setup popwin
(use-package popwin
  :init
  (progn
    (require 'popwin)
    (popwin-mode t))
  :bind ("C-'" . popwin:keymap)
  :config
  (progn
    (defvar popwin:special-display-config-backup popwin:special-display-config)
    (setq display-buffer-function 'popwin:display-buffer)

    (push '("*Completions*" :stick f :height 15 :position bottom :noselect t)
          popwin:special-display-config)
    (push '("*Warnings*" :stick t :height 15 :position bottom :noselect t)
          popwin:special-display-config)
    (push '(" *undo-tree*" :stick t :height 15 :position bottom :noselect t)
          popwin:special-display-config)
    (push '("*Comile-Log*" :stick f :noselect t)
          popwin:special-display-config)
    (push '("*eshell*" :stick t :height 15 :position bottom :noselect t)
          popwin:special-display-config)
    (push '("*git-gutter:diff*" :stick t :position bottom :height 15)
          popwin:special-display-config)))

;; setup company
(use-package company
  :diminish ""
  :config
  (progn
    (setq company-idle-delay 0.2
          company-minimum-prefix-length 3)))
(add-hook 'prog-mode-hook 'global-company-mode)

;; setup flycheck
(use-package flycheck
  :commands global-flycheck-mode
  :idle (global-flycheck-mode)
  :bind
  ("M-g M-n" . flycheck-next-error)
  ("M-g M-p" . flycheck-previous-error)
  ("M-g M-=" . flycheck-list-errors)
  :config
  (use-package flycheck-tip
    :config
    (add-hook 'flycheck-mode-hook
              (lambda ()
                (global-set-key (kbd "C-c C-n") 'flycheck-tip-cycle)
                (global-set-key (kbd "C-c C-p")
                                'flycheck-tip-cycle-reverse)))))

;; setup ido
(use-package ido
  :init (ido-mode t)
  :config
  (progn
    (setq ido-use-virtual-buffers nil
          ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-auto-merge-work-directories-length nil
          ido-create-new-buffer 'always
          ido-use-filename-at-point 'guess
          ido-max-prospects 10
          ido-save-directory-list-file
          (concat user-emacs-directory "misc/ido/.ido-list")))
  (use-package flx-ido
    :init (flx-ido-mode t)
    :config (setq ido-use-faces nil))
  (use-package ido-vertical-mode
    :init (ido-vertical-mode t)))

;; setup smex
(use-package smex
  :bind ("M-x" . smex)
  :config
  (setq smex-save-file (concat user-emacs-directory "misc/smex/.smex-items")))

;; setup expand-region
(use-package expand-region
  :config
  (progn
    (pending-delete-mode t))
  :bind (("C-c e" . er/expand-region)
         ("C-M-@" . er/contract-region)))

;; setup git-gutter
(use-package git-gutter
  :idle (global-git-gutter-mode t)
  :bind (("C-x =" . git-gutter:popup-hunk)
         ("C-c P" . git-gutter:previous-hunk)
         ("C-c N" . git-gutter:next-hunk)
         ("C-x p" . git-gutter:previous-hunk)
         ("C-x n" . git-gutter:next-hunk)
         ("C-c G" . git-gutter:popup-hunk)))

;; setup hideshow
(use-package hideshow
  :bind (("C-c TAB" . hs-toggle-hiding)
         ("C-\\" . hs-toggle-hiding)
         ("M-\\" . hs-hide-all)
         ("M-|" . hs-show-all)))
(defun my/enable-hs-minor-mode ()
  (interactive)
  (hs-minor-mode t))
(add-hook 'prog-mode-hook 'my/enable-hs-minor-mode)

;; setup rainbow-delimiters-mode
(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'global-rainbow-delimiters-mode))

;; setup smartparens
(use-package smartparens
  :config
  (progn
    (define-key sp-keymap (kbd "C-c (") 'sp-forward-barf-sexp)
    (define-key sp-keymap (kbd "C-c )") 'sp-forward-slurp-sexp)

    (sp-with-modes '(nxml-mode)
      (sp-local-pair "<" ">"))
    (add-hook 'prog-mode-hook 'smartparens-global-mode)))

;; setup org-mode
(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c t" . org-todo))
  :config
  (progn
    (setq org-directory "~/Org"
          org-completion-use-ido t
          ido-everywhere t)
    (setq org-todo-keywords
          '((sequence "TODO" "INPROGRESS" "WAITING" "DONE")))
    (setq org-todo-keyword-faces
          '(("TODO" :foreground "red")
            ("INPROGRESS" :foreground "blue")
            ("WAITING" :foreground "purple")
            ("DONE" :foreground "green")))
    (setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.9/libexec/ditaa0_9.jar")
    (org-babel-do-load-languages
     (quote org-babel-load-languages)
     (quote ((dot . t)
             (ditaa . t)
             (R . t)
             (gnuplot . t)
             (org . t)
             (latex . t))))
    (add-hook 'org-mode-hook (lambda()
                               (turn-on-auto-fill)
                               (turn-on-flyspell)))))
