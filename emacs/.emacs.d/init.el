(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(setq package-enable-at-startup nil)

;; setup the theme
(load-theme 'monokai t)

;; key bindings
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-x C-l") 'toggle-truncate-lines)
(global-set-key (kbd "C-x +") 'balance-windows-area)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-t") 'Control-X-prefix)
(defalias 'yes-or-no-p 'y-or-n-p)

;; default settings
(setq-default default-tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default find-file-visit-truename nil)
(setq-default find-file-visit-truename nil)
(put 'erase-buffer 'disabled nil)

;; make it purty... or at least not annoying
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq ring-bell-function (lambda()))
(setq inhibit-startup-message t initial-major-mode 'fundamental-mode)
(blink-cursor-mode -1)
(setq make-pointer-invisible t)
(line-number-mode t)
(column-number-mode t)
(setq echo-keystrokes 0.1)
(setq line-move-visual t)

;; miscellaneous settings
(setq read-file-name-completion-ignore-case t)
(setq backup-directory-alist `(("." . "~/.emacs.d/misc/.saves")))
(transient-mark-mode t)
(setq delete-auto-save-files t)
(when (window-system)
  (set-scroll-bar-mode 'nil)
  (mouse-wheel-mode t)
  (tooltip-mode -1))

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
(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)

;; setup c modes, used for school
(defun my/c-mode-init ()
  (c-set-style "k&r")
  (c-toggle-electric-state -1)
  (define-key c-mode-map (kbd "C-c o") 'ff-find-other-file)
  (define-key c++-mode-map (kbd "C-c o") 'ff-find-other-file)
  (hs-minor-mode 1)
  (setq c-basic-offset 4))
(add-hook 'c++-mode-hook #'my/c-mode-init)

;; find important comment words, function added later
(defun my/add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\)\\>"
          1 '((:foreground "#d7a3ad") (:weight bold)) t))))

;; remove tabs rfom the entire buffer
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

;; indents entire buffer, be careful doing this on large files
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

;; general settings that should be applied to all programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (subword-mode t)
            (my/add-watchwords)
            (global-flycheck-mode)))

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
  :init (global-flycheck-mode)
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
          ido-max-prospects 10)))
(use-package flx-ido
  :init (flx-ido-mode t)
  :config (setq ido-use-faces nil))
(use-package ido-vertical-mode
  :init (ido-vertical-mode t))

;; setup smex
(use-package smex
  :bind ("M-x" . smex))

;; setup expand-region
(use-package expand-region
  :config (pending-delete-mode t)
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

;; setup column-marker
(use-package column-marker
  :init (column-marker-1 80))

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
