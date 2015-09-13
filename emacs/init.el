(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://stable.melpa.org/packages/"))
(package-initialize)

(when (not (eq window-system nil))
  (set-fontset-font "fontset-default" 'symbol "Inconsolata")
  (set-default-font "Inconsolata")
  (set-face-attribute 'default nil :height 105)

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

(define-key key-translation-map "\C-t" "\C-x")

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-x C-l") 'toggle-truncate-lines)
(global-set-key (kbd "M-'") 'other-window)
(global-set-key (kbd "C-x m") 'eshell)

(setq-default line-number-mode t)
(setq-default column-number-mode t)
(setq-default transient-mark-mode t)
(setq-default menu-bar-mode t)
(setq-default tool-bar-mode t)
(setq-default blink-cursor-mode t)
(setq-default confirm-kill-emacs 'y-or-n-p)
(setq-default echo-keystrokes 0.1)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 79)
(setq-default vc-follow-symlinks t)
(setq-default read-file-name-completion-ignore-case t)
(setq-default delete-auto-save-files t)
(setq-default inhibit-startup-message t)
(setq-default make-backup-files nil)

(setq show-paren-mode t)
(setq global-font-lock-mode nil)


(defun my/add-watchwords ()
  "Highlight FIXME and TODO in code"
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\)\\>"
          1 '((:foreground "#DC8CC3")) t))))
(add-hook 'prog-mode-hook 'my/add-watchwords)

(defun my/c-mode-init()
  "Various style settings for C."
  (c-set-style "k&r")
  (c-toggle-electric-state -1)
  (setq c-basic-offset 4))
(add-hook 'c-mode-hook #'my/c-mode-init)

(defun my/go-mode-init()
  "Various style settings for Go."
  (setq c-basic-offset 8
        indent-tabs-mode t))
(add-hook 'go-mode-hook #'my/go-mode-init)

(defun my/nxml-mode-init()
  "Various style settings for nXML."
  (setq subword-mode t
        nxml-child-indent 4
        nxml-slash-auto-complete-flag t))
(add-hook 'nxml-mode-hook #'my/nxml-mode-init)

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))
(global-set-key [f11] 'toggle-fullscreen)

(defun untabify-buffer()
  "Replace all tabs with spaces."
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer()
  "Indent the buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Indent, untabify, and delete trailing whitespace."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(add-hook 'prog-mode-hook
          (lambda ()
            (visual-line-mode t)
            (subword-mode t)))

(load-theme 'solarized-dark t)

(require 'use-package)

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package smooth-scrolling
  :ensure t
  :config (setq smooth-scroll-margin 8))

(use-package undo-tree
  :ensure t
  :bind ("C-x u" . undo-tree-visualize))

(use-package magit
  :ensure t
  :bind ("M-g M-g" . magit-status)
  :config (setq magit-auto-rever-mode nil))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :bind (("C-c (" . sp-forward-barf-sexp)
         ("C-c )" . sp-forward-slurp-sexp))
  :config (use-package smartparens-config)
  :init (add-hook 'prog-mode-hook 'smartparens-mode t))

(use-package expand-region
  :ensure t
  :bind (("C-c e"   . er/expand-region)
         ("C-c C-e" . er/contract-region))
  :config (pending-delete-mode t))

(use-package hideshow
  :ensure t
  :bind (("C-c TAB" . hs-toggle-hiding)
         ("C-\\"    . hs-toggle-hiding)
         ("M-\\"    . hs-hide-all)
         ("M-|"     . hs-show-all))
  :init (add-hook 'prog-mode-hook 'hs-minor-mode t))

(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package company
  :ensure t
  :diminish company-mode
  :init (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 3
        company-selection-wrap-around t
        company-dabbrev-downcase nil
        company-transformers '(company-sort-by-occurrence)))

(use-package popwin
  :ensure t
  :init
  :config
  (progn
    (popwin-mode t)
    (defvar popwin:special-display-config-backup popwin:special-display-config)
    (setq display-buffer-function 'popwin:display-buffer)
    (push '("*Completions*" :stick f :height 20 :position bottom :noselect t)
          popwin:special-display-config)
    (push '("*Warnings*" :stick t :height 20 :position bottom :noselect t)
          popwin:special-display-config)
    (push '(" *undo-tree*" :stick t :height 20 :position bottom :noselect t)
          popwin:special-display-config)
    (push '("*Comile-Log*" :stick f :noselect t)
          popwin:special-display-config)))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config (setq-default js2-basic-offset 2))

(use-package dired
  :bind ("C-x C-j" . dired-jump)
  :config (use-package dired-x
            :config (setq ls-lisp-dirs-first t
                          delete-by-moving-to-trash t
                          dired-dwim-target t)
            (define-key dired-mode-map (kbd "C-c C-u") 'dired-up-directory)
            (define-key dired-mode-map (kbd "C-x C-q") 'wdired-change-to-wdired-mode)
            (add-hook 'dired-mode-hook (lambda ()
                                         (hl-line-mode t)
                                         (toggle-truncate-lines t)))))

(use-package helm
  :ensure t
  :bind (("C-x b" . helm-mini)
         ("C-x k" . helm-mini)
         ("M-x" . helm-M-x)
         ("C-x C-i" . helm-semantic-or-imenu)
         ("M-y" . helm-show-kill-ring)
         ("C-x f" . helm-find-files)
         ("C-h b" . helm-descbinds))
  :config (progn
            (use-package helm-files)
            (use-package helm-config)
            (setq helm-buffers-fuzzy-matching t
                  helm-truncate-lines t)
            (use-package helm-swoop
              :bind
              (("M-i" . helm-swoop)
               ("M-I" . helm-multi-swoop)))))

(use-package eww
  :ensure t
  :bind (("C-c w" . eww)
         ("C-c o" . eww-browse-with-external-browser)))

(use-package ido
  :ensure t
  :init (ido-mode t)
  :config
  (progn
    (setq ido-use-virtual-buffers nil
          ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-auto-merge-work-directories-length nil
          ido-create-new-buffer 'always
          ido-save-directory-list-file
          (concat user-emacs-directory "misc/ido/.ido")))
  (use-package flx-ido
    :ensure t
    :init (flx-ido-mode t)
    :config (setq ido-use-faces nil))
  (use-package ido-vertical-mode
    :ensure t
    :init (ido-vertical-mode t)))

(use-package org
  :ensure t
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-iswitchb)
         ("C-c l" . org-store-link))
  :init (progn
          (add-hook 'org-mode-hook (lambda ()
                                     (set-fill-column 79)
                                     (turn-on-auto-fill)))
          (setq org-capture-templates
                '(("t" "Todo" entry (file "~/org/refile.org")
                   "* TODO %?\n%U\n")))
          (setq org-todo-keyword-faces
                '(("TODO" :foreground "red")
                   ("NEXT" :foreground "red")
                   ("INPROGRESS" :foreground "yellow")
                   ("DONE" :foreground "green")
                   ("BLOCKED" :foreground "red" :weight bold)))
          (setq org-todo-keywords
                '((sequence "TODO(t)" "NEXT(n)" "INPROGRESS(i)" "DONE(d)" "|" "BLOCKED(b)")))
          (setq org-agenda-files '("~/org/work.org"
                                   "~/org/school.org"
                                   "~/org/refile.org"))
          (setq org-startup-indented t
                org-hide-leading-stars t
                org-clock-persist 'history)
          (org-clock-persistence-insinuate)))
