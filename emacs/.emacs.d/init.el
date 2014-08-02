(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(setq package-enable-at-startup nil)

;; setup the theme
(load-theme 'molokai t)

;; key bindings
(define-key key-translation-map "\C-t" "\C-x")
(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-x C-l") 'toggle-truncate-lines)
(global-set-key (kbd "C-x +") 'balance-windows-area)
(global-set-key (kbd "C-;") 'iedit-mode)
(defalias 'yes-or-no-p 'y-or-n-p)

;; default settings
(setq-default default-tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default find-file-visit-truename nil)

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

;; setup c modes
(defun my/c-mode-init ()
  (c-set-style "k&r")
  (c-toggle-electric-state -1)
  (define-key c-mode-map (kbd "C-c o") 'ff-find-other-file)
  (define-key c++-mode-map (kbd "C-c o") 'ff-find-other-file)
  (hs-minor-mode 1)
  (setq c-basic-offset 4))
(add-hook 'c++-mode-hook #'my/c-mode-init)

;; miscellaneous settings
(transient-mark-mode t)
(setq delete-auto-save-files t)
(setq read-file-name-completion-ignore-case t)
(when (window-system)
  (set-scroll-bar-mode 'nil)
  (mouse-wheel-mode t)
  (tooltip-mode -1))

;; find important comment words, function added later
(defun my/add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\)\\>"
          1 '((:foreground "#d7a3ad") (:weight bold)) t))))

;; because why not?
(defun my/insert-lod ()
  "Well. This is disappointing."
  (interactive)
  (insert "ಠ_ಠ"))
(global-set-key (kbd "C-c M-d") 'my/insert-lod)

;; tabs are the works
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
    (push '("*Warnings*" :stick f :height 15 :position bottom :noselect t)
          popwin:special-display-config)
    (push '(" *undo-tree*" :stick t :height 15 :position bottom :noselect t)
          popwin:special-display-config)))

;; setup auto-complete
(use-package auto-complete
  :init
  (progn
    (use-package popup)
    (use-package fuzzy)
    (use-package auto-complete-config)
    (setq ac-comphist-file
          (concat user-emacs-directory "misc/ac-comphist.dat"))
    (global-auto-complete-mode t)
    (ac-config-default))
  :config
  (progn
    (define-key ac-complete-mode-map (kbd "M-n") 'ac-next)
    (define-key ac-complete-mode-map (kbd "M-p") 'ac-previous)
    (define-key ac-complete-mode-map (kbd "C-s") 'ac-isearch)
    (define-key ac-completing-map (kbd "<tab>") 'ac-complete)))

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

