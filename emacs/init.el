;;; init.el --- Summary

;;; Commentary:

;;; Code:
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(add-to-list 'load-path "/home/shellhead/.dotfiles/emacs/org-reveal")

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
(global-set-key (kbd "C-x C-l") 'toggle-truncate-lines)
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
(setq initial-major-mode 'fundamental-mode)

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
  "Toggle full screen in x server based environment."
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

(load-theme 'spacegray t)

(require 'use-package)

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

(use-package magit
  :ensure t
  :init
  (evil-leader/set-key
    "G" 'magit-status))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :bind
  (("M-g n" . git-gutter:next-hunk)
   ("M-g p" . git-gutter:previous-hunk))
  :init
  (global-git-gutter-mode t))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :bind (("C-c (" . sp-forward-barf-sexp)
         ("C-c )" . sp-forward-slurp-sexp))
  :init (add-hook 'prog-mode-hook 'smartparens-mode t)
  :config (use-package smartparens-config))

(use-package expand-region
  :ensure t
  :init
  (evil-leader/set-key
    "e" 'er/expand-region
    "E" 'er/contract-region))

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
  (setq js2-global-externs '("describe" "xdescribe" "it" "xit" "beforeEach" "afterEach" "before" "after")))

(use-package dired
  :config
  (use-package dired-x)
  (use-package dired-narrow
    )
  (evil-leader/set-key
    "d" 'dired-jump)
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
  (("C-M-z" . helm-resume)
   ("C-c C-o" . helm-occur)
   ("M-y" . helm-show-kill-ring)
   ("C-h a" . helm-apropos)
   ("C-h m" . helm-man-woman)
   ("C-h SPC" . helm-all-mark-rings)
   ("C-x C-i" . helm-semantic-or-imenu)
   ("C-h b" . helm-descbinds)
   ("M-=" . helm-yas-complete)
   ("M-i" . helm-swoop)
   ("M-I" . helm-multi-swoop))
  :config  
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action))

(use-package eww
  :ensure t
  :bind
  (("C-c w" . eww)
   ("C-c o" . eww-browse-with-external-browser)))

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
  (load-library "ox-reveal")
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
  (projectile-global-mode))

;; download from https://github.com/djcb/mu
(use-package mu4e
  :if (eq window-system 'x)
  :config
  (evil-leader/set-key
    "m" 'mu4e)
  (setq mu4e-mu-binary (executable-find "mu"))
  (setq mu4e-html2text-command (concat
                                (executable-find "elinks") " -dump"))
  (setq mu4e-get-mail-command (executable-find "offlineimap"))
  (setq send-mail-function 'sendmail-send-it)
  (setq message-kill-buffer-on-exit t)
  (setq sendmail-program (executable-find "msmtp"))
  (setq smtpmail-queue-mail t)
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-maildir "~/.mail")
  (setq smtpmail-queue-dir "/.mail/queue")
  (setq mu4e-attachment-dir "~/Downloads")
  (setq mu4e-sent-messages-behavior 'delete)
  (setq mu4e-update-interval 300)
  (setq mu4e-view-show-images t)
  (setq mu4e-compose-signature "Cheers, Mike")
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "gmail"
            :match-func
            (lambda (msg)
              (when msg
                (mu4e-message-contact-field-matches
                 msg :to "mike.hunsinger@gmail.com")))
            :vars '((user-mail-address . "mike.hunsinger@gmail.com" )
                    (user-full-name . "Michael Hunsinger")
                    (mu4e-sent-folder . "/gmail/[Gmail].Sent Mail/")
                    (mu4e-drafts-folder . "/gmail/[Gmail].Drafts")
                    (mu4e-trash-folder . "/gmail/[Gmail].Trash")))
          ,(make-mu4e-context
            :name "school"
            :enter-func (lambda ()
                          (mu4e-message "Switch to the Work context"))
            :match-func
            (lambda (msg)
              (when msg
                (mu4e-message-contact-field-matches
                 msg :to "michael.hunsinger@ucdenver.edu")))
            :vars '((user-mail-address . "michael.hunsinger@ucdenver.edu")
                    (user-full-name . "Michael Hunsinger")
                    (mu4e-sent-folder . "/school/Sent/")
                    (mu4e-drafts-folder . "/school/Drafts")
                    (mu4e-trash-folder . "/school/Trash")))))
  (define-key mu4e-view-mode-map (kbd "j") 'next-line)
  (define-key mu4e-view-mode-map (kbd "k") 'previous-line)
  (define-key mu4e-headers-mode-map (kbd "J") 'mu4e~headers-jump-to-maildir)
  (define-key mu4e-headers-mode-map (kbd "j") 'next-line)
  (define-key mu4e-headers-mode-map (kbd "k") 'previous-line))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets" "~/.emacs.d/misc"))
  (yas-global-mode t))

(use-package avy
  :config
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

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
                "--ignore=4"))))

(use-package nxml-mode
  :mode "\\.xml|wsdl|xsd\\'")

(use-package evil-escape
  :demand
  :ensure t
  :init
  (evil-escape-mode)
  :config
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.2)
  (setq-default evil-escape-unordered-key-sequence t))

(use-package evil
  :ensure t
  :config
  (evil-mode t)
  ;; key bindings for dvorak
  ;; normal state
  (define-key evil-normal-state-map "a" 'evil-beginning-of-line)
  (define-key evil-normal-state-map "e" 'evil-end-of-line)
  (define-key evil-normal-state-map "E" 'evil-append-line)
  (define-key evil-normal-state-map "U" 'redo)
  ;; motion state
  (define-key evil-motion-state-map "h" 'evil-backward-char)
  (define-key evil-motion-state-map "H" 'evil-backward-word-begin)
  (define-key evil-motion-state-map "t" 'evil-forward-char)
  (define-key evil-motion-state-map "T" 'evil-forward-word-begin))

(use-package evil-magit
  :init (evil-magit-init))

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "x" 'helm-M-x
    "b" 'helm-buffers-list
    "h" 'helm-mini
    "f" 'helm-find-files))
;;; init.el ends here
