;; init.el --- Initialization file for Emacs
;; Many thanks to Lee for helping me with Emacs

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(setq package-enable-at-startup nil)

(defconst f '(-1))

;; Default emacs settings
(setq inhibit-startup-message      t
      initial-major-mode           'fundamental-mode)
(setq mac-command-modifier 'super)
(setq ring-bell-function           (lambda()))
(put 'erase-buffer                 'disabled nil)

;; File reading/writing settings
(setq read-file-name-completion-ignore-case t)
(setq delete-auto-save-files                t)
(setq find-file-visit-truename              nil)

;; Appearances
(load-theme 'smyx                  t)
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
(setq vc-handled-backends          '(SVN Git))

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
  (setq subword-mode t))
(add-hook 'nxml-mode-hook #'my/nxml-mode-init)

;; Sync Emacs' kill ring and Mac's copy buffer
(when (eq system-type 'darwin)
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

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))

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
  (use-package smartparens-config)
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
         ("C-c C-e" . er/contract-region))
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
  :config
  (popwin-mode t)
  (progn
    (defvar popwin:special-display-config-backup popwin:special-display-config)
    (setq display-buffer-function 'popwin:display-buffer)
    (push '("*Completions*" :stick f :height 20 :position bottom :noselect t)
          popwin:special-display-config)
    (push '("*Warnings*" :stick t :height 20 :position bottom :noselect t)
          popwin:special-display-config)
    (push '(" *undo-tree*" :stick t :height 20 :position bottom :noselect t)
          popwin:special-display-config)
    (push '("*Comile-Log*" :stick f :noselect t)
          popwin:special-display-config)
    (push '("*eshell*" :stick t :height 20 :position bottom :noselect t)
          popwin:special-display-config)))

;; git-gutter
(use-package git-gutter
  :idle (git-gutter-mode t)
  :bind (("C-x =" . git-gutter:popup-hunk)
         ("C-c P" . git-gutter:previous-hunk)
         ("C-c N" . git-gutter:next-hunk)
         ("C-x p" . git-gutter:previous-hunk)
         ("C-x n" . git-gutter:next-hunk)
         ("C-c G" . git-gutter:popup-hunk)))

;; flycheck
(use-package flycheck
  :init
  (flycheck-mode)
  (setq-default flycheck-disabled-checkers
		'(emacs-lisp-checkdoc))
  (use-package flycheck-tip
    :config
    (add-hook 'flycheck-mode-hook
	      (lambda ()
		(global-set-key (kbd "C-c C-n") 'flycheck-tip-cycle)
		(global-set-key (kbd "C-c C-p") 'flycheck-tip-cycle-reverse)))))

;; js2-mode
(use-package js2-mode
  :mode "\\.js\\'")

;; dired
(use-package dired
  :bind
  ("C-c ." . dired-jump)
  :config
    (use-package dired-x
      :config
      (when (eq system-type 'darwin)
	(add-to-list 'dired-omit-extensions ".DS_Store")
	(setq insert-directory-program "/usr/local/bin/gls")
	(setq dired-listing-switches "-aBhl --group-directories-first"))
      (setq ls-lisp-dirs-first t
	    delete-by-moving-to-trash t
	    dired-dwim-target t)
      (define-key dired-mode-map (kbd "C-c C-u") 'dired-up-directory)
      (define-key dired-mode-map (kbd "C-x C-q") 'wdired-change-to-wdired-mode)
      (add-hook 'dired-mode-hook (lambda ()
				   (hl-line-mode t)
				   (toggle-truncate-lines t)))))

;; golden-ratio
(use-package golden-ratio
  :idle
  (golden-ratio-mode t))

;; multiple-cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-c >" . mc/mark-next-like-this)
         ("C-c <" . mc/mark-previous-like-this)
         ("C-c C-a" . mc/mark-all-like-this)))
