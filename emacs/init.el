;;; init.el --- Summary

;;; Commentary:

;;; Code:
(setq-default user-full-name "Michael Hunsinger")

(when window-system
  (set-fontset-font "fontset-default" 'symbol "Inconsolata")
  (set-frame-font "Inconsolata")
  (face-remap-add-relative 'default :height 2.0)
  (when (functionp 'menu-bar-mode)
    (menu-bar-mode -1))
  (when (functionp 'set-scroll-bar-mode)
    (set-scroll-bar-mode nil))
  (when (functionp 'mouse-wheel-mode)
    (mouse-wheel-mode -1))
  (when (functionp 'tooltip-mode)
    (tooltip-mode -1))
  (when (functionp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (functionp 'blink-cursor-mode)
    (blink-cursor-mode -1)))

(defalias 'yes-or-no-p 'y-or-n-p)

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
(setq ring-bell-function (lambda ()))

(defun my/nxml-mode-init ()
  (setq nxml-child-indent 4)
  (setq nxml-slash-auto-complete-flag t))
(add-hook 'nxml-mode-hook #'my/nxml-mode-init)

(defun toggle-fullscreen ()
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))
(global-set-key [f11] 'toggle-fullscreen)

(defun untabify-buffer()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

;; package setup
(require 'package)

;; package repositories
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(require 'use-package)

(use-package zenburn-theme
  :ensure t
  :init (load-theme 'zenburn t))

(use-package diminish
  :ensure t)

(use-package delight
  :ensure t)

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package smooth-scrolling
  :ensure t
  :config (setq smooth-scroll-margin 4))

(use-package magit
  :ensure t
  :init
  (use-package evil-magit
    :ensure t
    :init
    (evil-magit-init)))

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
    :ensure t
    :config
    (define-key dired-mode-map (kbd "/") 'dired-narrow))
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
  (("M-x" . helm-M-x)
   ("C-M-z" . helm-resume)
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

(use-package helm-descbinds
  :ensure t)

(use-package helm-projectile
  :ensure t
  :config
  (setq helm-projectile-fuzzy-match nil))

(use-package eww
  :ensure t
  :bind
  (("C-c w" . eww)
   ("C-c o" . eww-browse-with-external-browser)))

(use-package alert
  :config
  (when (eq system-type 'gnu/linux)
    (setq alert-default-style 'notifications)))

(use-package org
  :ensure t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (use-package evil-org
    :ensure t)
  (setq org-list-allow-alphabetical t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-plantuml-jar-path "/usr/share/java/plantuml.jar")
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((plantuml . t)
                                 (dot . t)
                                 (gnuplot . t))))

(use-package projectile
  :commands projectile-global-mode
  :diminish projectile-mode
  :config
  (projectile-global-mode))

;; download from https://github.com/djcb/mu
(use-package mu4e
  :if (executable-find "mu")
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  :config
  (use-package mu4e-alert
    :config
    (setq mu4e-alert-set-default-style 'notifications))
  (use-package evil-mu4e
    :ensure t)
  (setq mu4e-mu-binary              (executable-find "mu")
        mu4e-get-mail-command       (executable-find "offlineimap")
        sendmail-program            (executable-find "msmtp")
        send-mail-function          'sendmail-send-it
        mail-user-agent             'mu4e-user-agent
        mu4e-update-interval        60
        mu4e-maildir                "~/.mail"
        smtpmail-queue-dir          "~/.mail/queue"
        mu4e-attachment-dir         "~/Downloads"
        mu4e-sent-messages-behavior 'delete
        mu4e-view-show-images       t
        mu4e-html2text-command      (concat (executable-find "elinks")
                                            " -dump -dump-width 80")
        mu4e-compose-signature      "Cheers,\nMike"
        mu4e-contexts
        `(,(make-mu4e-context
            :name "gmail"
            :match-func
            (lambda (msg)
              (when msg
                (mu4e-message-contact-field-matches
                 msg :to "mike.hunsinger@gmail.com")))
            :vars '((user-mail-address . "mike.hunsinger@gmail.com" )
                    (mu4e-sent-folder . "/gmail/[Gmail].Sent Mail/")
                    (mu4e-drafts-folder . "/gmail/[Gmail].Drafts")
                    (mu4e-trash-folder . "/gmail/[Gmail].Trash")))
          ,(make-mu4e-context
            :name "school"
            :match-func
            (lambda (msg)
              (when msg
                (mu4e-message-contact-field-matches
                 msg :to "michael.hunsinger@ucdenver.edu")))
            :vars '((user-mail-address . "michael.hunsinger@ucdenver.edu")
                    (mu4e-sent-folder . "/school/Sent/")
                    (mu4e-drafts-folder . "/school/Drafts")
                    (mu4e-trash-folder . "/school/Trash"))))))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets" "~/.emacs.d/misc"))
  (yas-global-mode t))

(use-package flyspell
  :if (executable-find "aspell")
  :init (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :config
  (setq ispell-program-name (executable-find "aspell"))
  (setq ispell-extra-args
        (list "--sug-mode=fast"
              "--lang=en_US"
              "--ignore=4")))

(use-package nxml-mode
  :mode "\\.xml|wsdl|xsd\\'")

(use-package evil-escape
  :ensure t
  :diminish evil-escape-mode
  :init
  (evil-escape-mode)
  :config
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.2)
  (setq-default evil-escape-unordered-key-sequence t))

(use-package evil
  :ensure t
  :config
  (use-package evil-surround
    :ensure t
    :config (evil-surround-mode))
  (use-package evil-easymotion
    :ensure t
    :config
    (evilem-default-keybindings "SPC"))
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "e" 'eshell
      "x" 'helm-M-x
      "b" 'helm-buffers-list
      "H" 'helm-mini
      "f" 'helm-find-files
      "d" 'dired-jump
      "p" 'helm-projectile
      "g" 'helm-projectile-grep
      "F" 'helm-projectile-find-file
      "ci" 'evilnc-comment-or-uncomment-lines
      "cr" 'comment-or-uncomment-region
      "G" 'magit-status
      "m" 'mu4e))
  (diminish 'undo-tree-mode)
  (evil-mode t))

(use-package evil-nerd-commenter
  :ensure t)

(use-package evil-matchit
  :ensure t
  :config (global-evil-matchit-mode))
;; init.el ends here
