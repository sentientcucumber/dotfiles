;;; init.el --- Initialization file for Emacs
;;; Many thanks to Lee for helping me with Emacs

;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(setq package-enable-at-startup nil)

(defconst f '(-1))

;; Default emacs settings
(setq inhibit-startup-message      t
      initial-major-mode           'fundamental-mode)
(setq ring-bell-function           (lambda()))
(put 'erase-buffer                 'disabled nil)

;; File reading/writing settings
(setq read-file-name-completion-ignore-case t)
(setq delete-auto-save-files                t)
(setq find-file-visit-truename              nil)

;; Appearances
(load-theme 'subatomic256          t)
(global-font-lock-mode             t)
(line-number-mode                  t)
(column-number-mode                t)
(transient-mark-mode               t)
(menu-bar-mode                     f)
(tool-bar-mode                     f)
(blink-cursor-mode                 f)

(setq vc-follow-symlinks           t)
(setq make-pointer-invisible       t)
(setq fill-column                  80)
(setq echo-keystrokes              0.1)

;; Key Bindings
(global-set-key (kbd "C-s")        'isearch-forward-regexp)
(global-set-key (kbd "C-r")        'isearch-backward-regexp)
(global-set-key (kbd "M-%")        'query-replace-regexp)
(global-set-key (kbd "C-x C-l")    'toggle-truncate-lines)
(global-set-key (kbd "C-x +")      'balance-windows-area)
(global-set-key (kbd "RET")        'newline-and-indent)
(global-set-key (kbd "C-t")        'Control-X-prefix)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Custom variable
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ido-max-prospects 10)
 '(nxml-child-indent 4)
 '(nxml-slash-auto-complete-flag t))

;; General settings that should be applied to all programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (visual-line-mode      t)
            (subword-mode          t)))

;; C
(defun my/c-mode-init()
  (c-set-style                     "k&r")
  (c-toggle-electric-state         f)
  (hs-minor-mode                   t)
  (setq c-basic-offset             4))
(add-hook 'c-mode-hook #'my/c-mode-init)

;; Go
(defun my/go-mode-init()
  (setq c-basic-offset             8
        indent-tabs-mode           t))
(add-hook 'go-mode-hook #'my/go-mode-init)

;; XML
(defun my/nxml-mode-init()
  (setq subword-mode               t))
(add-hook 'nxml-mode-hook #'my/nxml-mode-init)

;; Sync Emacs' kill ring and Mac's copy buffer
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

;; stop putting temp files in the same directory, if I need it put it elsewhere
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Fixed issues on loading
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)

;; Remove tabs from the entire buffer
(defun untabify-buffer()
  (interactive)
  (untabify (point-min) (point-max)))

;; Indents entire buffer, be careful doing this on large files
(defun indent-buffer()
  (interactive)
  (indent-region (point-min) (point-max)))

;; Packages
(require 'use-package)

;; uniquify
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; smooth-scrolling
(use-package smooth-scrolling
  :config
  (setq smooth-scroll-margin 8))

;; undo-tree
(use-package undo-tree
  :bind
  ("C-x u" . undo-tree-visualize))

;; magit
(use-package magit
  :bind ("M-g M-g" . magit-status))

;; smartparens
(use-package smartparens
  :bind
  ("C-c (" . sp-forward-barf-sexp)
  ("C-c )" . sp-forward-slurp-sexp)
  :config
  (sp-pair "'" nil :actions :rem)
  :init
  (add-hook 'prog-mode-hook 'smartparens-mode t))

;; smex
(use-package smex
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
  :config
  (setq smex-save-file (concat user-emacs-directory "misc/smex/.smex-items")))

;; expand-region
(use-package expand-region
  :bind (("C-c e" . er/expand-region)
         ("C-M-@" . er/contract-region))
  :config
  (pending-delete-mode t))

;; hideshow
(use-package hideshow
  :bind
  ("C-c TAB" . hs-toggle-hiding)
  ("C-\\" . hs-toggle-hiding)
  ("M-\\" . hs-hide-all)
  ("M-|" . hs-show-all)
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode t))

;; rainbow-delimiters
(use-package rainbow-delimiters
   :init
   (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; ido
(use-package ido
  :init
  (ido-mode t)
  :config
  (progn
    (setq ido-use-virtual-buffers                nil
          ido-enable-prefix                      nil
          ido-enable-flex-matching               t
          ido-auto-merge-work-directories-length nil
          ido-create-new-buffer                  'always
          ido-use-filename-at-point              nil
          ido-max-prospects                      10
          ido-save-directory-list-file
          (concat user-emacs-directory "misc/ido/.ido-list")))
  (use-package flx-ido
    :init
    (flx-ido-mode t)
    :config
    (setq ido-use-faces nil))
  (use-package ido-vertical-mode
    :init
    (ido-vertical-mode t)))

;; company
(use-package company
  :init
  (setq company-idle-delay            0.1
	company-minimum-prefix-length 3
	company-selection-wrap-around t
	company-show-numbers          t
	company-dabbrev-downcase      nil
	company-transformers          '(company-sort-by-occurrence))
  (bind-keys :map company-active-map
	     ("C-n"      . company-select-next)
	     ("C-p"      . company-select-previous)
	     ("C-d"      . company-show-doc-buffer)
	     ("<tab>"    . company-complete))
  (add-hook 'prog-mode-hook 'company-mode))

;; popwin
(use-package popwin
  :init
  (popwin-mode t)
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

;; git-gutter
(use-package git-gutter
  :idle (global-git-gutter-mode t)
  :bind (("C-x =" . git-gutter:popup-hunk)
         ("C-c P" . git-gutter:previous-hunk)
         ("C-c N" . git-gutter:next-hunk)
         ("C-x p" . git-gutter:previous-hunk)
         ("C-x n" . git-gutter:next-hunk)
         ("C-c G" . git-gutter:popup-hunk)))

;; org
(use-package org
  :bind
  ("C-c a" . org-agenda)
  ("C-c l" . org-store-link)
  ("C-c t" . org-todo)
  :config
  (progn
    (setq org-directory "~/Org"
          org-completion-use-ido t
          ido-everywhere         t)
    (setq org-todo-keywords
          '((sequence "TODO" "INPROGRESS" "WAITING" "DONE")))
    (setq org-todo-keyword-faces
          '(("TODO"       :foreground "red")
            ("INPROGRESS" :foreground "yellow")
            ("WAITING"    :foreground "yellow")
            ("DONE"       :foreground "green")))
    (setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.9/libexec/ditaa0_9.jar")
    (org-babel-do-load-languages
     (quote org-babel-load-languages)
     (quote ((dot     . t)
             (ditaa   . t)
             (R       . t)
             (gnuplot . t)
             (org     . t)
             (latex   . t))))
    (add-hook 'org-mode-hook (lambda()
                               (turn-on-auto-fill)
                               (turn-on-flyspell)))))
