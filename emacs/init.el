;;; init.el --- Summary

;;; Commentary:

;;; Code:
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

;; general settings
(setq-default user-full-name "Michael Hunsinger")
(setq-default user-mail-address "mike.hunsinger@gmail.com")

;; window settings
(when (not (eq window-system nil))
  (set-fontset-font "fontset-default" 'symbol "Inconsolata")
  (set-frame-font "Inconsolata")
  (set-face-attribute 'default nil :height 120)
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

;; keybindings
(define-key key-translation-map "\C-x" "\C-t")
(define-key key-translation-map "\C-t" "\C-x")
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-x C-l") 'toggle-truncate-lines)
(global-set-key (kbd "M-'") 'other-window)
(global-set-key (kbd "C-x m") 'eshell)

;; global values
(setq-default line-number-mode t)
(setq-default column-number-mode t)
(setq-default transient-mark-mode t)
(setq-default menu-bar-mode t)
(setq-default tool-bar-mode t)
(setq-default blink-cursor-mode t)
(setq-default inhibit-startup-message t)
(setq-default confirm-kill-emacs 'y-or-n-p)
(setq-default echo-keystrokes 0.1)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 79)
(setq-default vc-follow-symlinks t)
(setq-default read-file-name-completion-ignore-case t)
(setq-default delete-auto-save-files t)
(setq-default make-backup-files nil)

;; hooks
(defun my/nxml-mode-init ()
  "Various style settings for XML."
  (setq nxml-child-indent 4)
  (setq nxml-slash-auto-complete-flag t))
(add-hook 'nxml-mode-hook #'my/nxml-mode-init)

(add-hook 'prog-mode-hook
          (lambda ()
            (visual-line-mode t)
            (hl-line-mode t)))

;; various functions
(defun toggle-fullscreen ()
  "Toggle full screen"
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

;; package setup
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(require 'use-package)

(use-package solarized-theme
  :ensure t
  :init
  (load-theme 'solarized-dark t))

(use-package delight
  :ensure t)

(use-package subword
  :diminish subword-mode
  :init (global-subword-mode))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package smooth-scrolling
  :ensure t
  :config (setq smooth-scroll-margin 4))

(use-package undo-tree
  :ensure t
  :bind ("C-x u" . undo-tree-visualize)
  :config
  (setq undo-tree-visualizer-diff t))

(use-package magit
  :ensure t
  :bind ("M-g M-g" . magit-status))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :bind
  (("M-g n" . git-gutter:next-hunk)
   ("M-g p" . git-gutter:previous-hunk))
  :init
  (global-git-gutter-mode t))

;; (electric-pair-mode 1)
;; (setq electric-pair-preserve-balance t
;;       electric-pair-delete-adjacent-pairs t
;;       electric-pair-open-newline-between-pairs nil)
;; (show-paren-mode 1)

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :bind (("C-c (" . sp-forward-barf-sexp)
         ("C-c )" . sp-forward-slurp-sexp))
  :init (add-hook 'prog-mode-hook 'smartparens-mode t)
  :config (use-package smartparens-config))

(use-package expand-region
  :ensure t
  :bind (("C-c e"   . er/expand-region)
         ("C-c C-e" . er/contract-region))
  :config (pending-delete-mode t))

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
  :config
  (popwin-mode t)
  (defvar popwin:special-display-config-backup popwin:special-display-config)
  (setq display-buffer-function 'popwin:display-buffer)
  (push '("*Completions*" :stick f :height 20 :position bottom :noselect t)
        popwin:special-display-config)
  (push '("*Warnings*" :stick t :height 20 :position bottom :noselect t)
        popwin:special-display-config)
  (push '(" *undo-tree*" :stick t :height 20 :position bottom :noselect t)
        popwin:special-display-config)
  (push '("*Compile-Log*" :stick f :noselect t)
        popwin:special-display-config))

(use-package js2-mode
  :ensure t
  :delight js2-mode "js2"
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2)
  (setq js2-include-node-externs t)
  (setq js2-indent-switch-body t)
  (setq js-indent-level 2)
  (setq js2-global-externs '("describe" "xdescribe" "it" "xit" "beforeEach" "afterEach")))

(use-package dired
  :bind ("C-x C-j" . dired-jump)
  :config
  (use-package dired-x)
  (setq ls-lisp-dirs-first t)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "C-M-u") 'dired-up-directory)
  (define-key dired-mode-map (kbd "C-x C-q") 'wdired-change-to-wdired-mode)
  (add-hook 'dired-mode-hook (lambda ()
                               (hl-line-mode t)
                               (toggle-truncate-lines t))))

(use-package helm
  :ensure t
  :bind
  (("C-x b" . helm-mini)
   ("C-x k" . helm-mini)
   ("M-x" . helm-M-x)
   ("C-x C-i" . helm-semantic-or-imenu)
   ("M-y" . helm-show-kill-ring)
   ("C-x f" . helm-find-files)
   ("C-h b" . helm-descbinds))
  :config
  (use-package helm-files)
  (use-package helm-config)
  (use-package helm-swoop
    :bind
    (("M-i" . helm-swoop)
     ("M-I" . helm-multi-swoop)))
  (setq helm-buffers-fuzzy-matching t
        helm-truncate-lines t)
  (use-package helm-c-yasnippet
    :bind ("M-=" . helm-yas-complete)))

(use-package eww
  :ensure t
  :bind
  (("C-c w" . eww)
   ("C-c o" . eww-browse-with-external-browser)))

(use-package ido
  :ensure t
  :init (ido-mode t)
  :config
  (use-package ido-ubiquitous)
  (use-package flx-ido
    :ensure t
    :init (flx-ido-mode t)
    :config (setq ido-use-faces nil))
  (use-package ido-vertical-mode
    :ensure t
    :init (ido-vertical-mode t))
  (setq ido-use-virtual-buffers nil)
  (setq ido-enable-prefix nil)
  (setq ido-enable-flex-matching t)
  (setq ido-auto-merge-work-directories-length nil)
  (setq ido-create-new-buffer 'always)
  (setq ido-save-directory-list-file
        (concat user-emacs-directory "misc/ido/.ido")))

(use-package alert
  :defer t
  :config
  (when (eq system-type 'gnu/linux)
    (setq alert-default-style 'notifications)))

(use-package org
  :ensure t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (defun my/org-mode-hook ()
    (setq fill-column 79)
    (turn-on-auto-fill)
    (turn-on-flyspell))
  (add-hook 'org-mode-hook #'my/org-mode-hook)

  ;; special keybindings for org-mode only
  (define-key org-mode-map (kbd "C-c t") 'org-todo)
  ;; agenda
  (setq org-agenda-files '("~/org/work.org"
                           "~/org/personal.org"))
  ;; randomness
  (setq org-list-allow-alphabetical t)
  (org-display-inline-images t)
  (add-to-list 'org-export-backends '(odt))
  (setq org-confirm-babel-evaluate nil)
  (setq org-plantuml-jar-path "/usr/share/java/plantuml.jar")
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((plantuml . t)
                                 (dot . t)
                                 (gnuplot . t))))

(use-package projectile
  ;; checkout perspective at some point
  :defer 5
  :commands projectile-global-mode
  :diminish projectile-mode
  :config
  (projectile-global-mode)
  (use-package helm-projectile
    :config (helm-projectile-on)))

;; download from https://github.com/djcb/mu
(use-package mu4e
  :if (eq window-system 'x)
  :bind ("C-c m" . mu4e)
  :config
  (setq mu4e-mu-binary (executable-find "mu"))
  (setq mu4e-html2text-command (concat (executable-find "html2text") " -utf8 -width 72"))
  (setq mu4e-get-mail-command (executable-find "offlineimap"))

  (setq send-mail-function 'sendmail-send-it)
  (add-hook 'mu4e-view-mode-hook 'visual-line-mode)
  (setq message-kill-buffer-on-exit t)
  (setq sendmail-program (executable-find "msmtp"))
  (setq smtpmail-queue-mail t)
  (setq mail-user-agent 'mu4e-user-agent)

  (setq mu4e-maildir "~/.mail")
  (setq mu4e-drafts-folder "/[Gmail].Drafts")
  (setq mu4e-sent-folder "/[Gmail].Sent Mail")
  (setq mu4e-trash-folder "/[Gmail].Trash")
  (setq smtpmail-queue-dir "/.mail/queue")
  (setq mu4e-attachment-dir "~/Downloads")

  (setq mu4e-sent-messages-behavior 'delete)
  (setq mu4e-maildir-shortcuts
        '(("/INBOX" . ?i)
          ("/[Gmail].Sent Mail" . ?s)
          ("/[Gmail].Trash" . ?t)
          ("/[Gmail].All Mail" . ?a)))
  (setq mu4e-update-interval 300)
  (setq mu4e-view-show-images t)

  (setq mu4e-user-mail-address-list '("mike.hunsinger@gmail.com"))
  (setq mu4e-compose-signature "Cheers, Mike"))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets" "~/.emacs.d/misc"))
  (yas-global-mode t))

(use-package avy
  :bind ("C-=" . avy-goto-char)
  :config
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

(use-package flycheck
  :bind (("M-g M-n" . flycheck-next-error)
         ("M-g M-p" . flycheck-previous-error)
         ("M-g M-=" . flycheck-list-errors))
  :init (global-flycheck-mode)
  :diminish flycheck-mode
  :config
  (setq flycheck-javascript-eslint-executable "/home/shellhead/.npm-packages/bin/eslint")
  (setq flycheck-eslintrc "/home/shellhead/development/swagger-lint/.eslintrc"))

(use-package flyspell
  :defer t
  :init (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :config
  (when (executable-find "aspell")
    (setq ispell-program-name (executable-find "aspell"))
    (setq ispell-extra-args
          (list "--sug-mode=fast"
                "--lang=en_US"
                "--ignore=4")))
  (use-package helm-flyspell
    :init
    (define-key flyspell-mode-map (kbd "M-S") #'helm-flyspell-correct)))

;;; init.el ends here
