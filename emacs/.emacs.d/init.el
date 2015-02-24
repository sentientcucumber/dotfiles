;; init.el --- A masterpiece in progress
;; Many thanks to Lee for helping me with this

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defconst f '(-1))

;; Default emacs settings
(setq-default package-enable-at-startup nil
              inhibit-startup-message t
              initial-major-mode 'fundamental-mode
              ring-bell-function (lambda())
              indent-tabs-mode nil
              fill-column 79)

;; File reading/writing settings
(setq read-file-name-completion-ignore-case t
      delete-auto-save-files t
      find-file-visit-truename nil
      vc-follow-symlinks t
      electric-indent-mode f
      show-paren-mode t)

;; Appearances
(load-theme 'smyx             t)
(global-font-lock-mode        t)
(line-number-mode             t)
(column-number-mode           t)
(transient-mark-mode          t)
(menu-bar-mode                f)
(tool-bar-mode                f)
(blink-cursor-mode            f)
(setq make-pointer-invisible  t
      echo-keystrokes         0.1
      vc-handled-backends     '(SVN Git))

;; Window settings
(when (eq window-system 'x)
  ;; Font
  (set-fontset-font "fontset-default" 'symbol "Fantasque Sans Mono")
  (set-default-font "Fantasque Sans Mono")
  (set-face-attribute 'default nil :height 90)

  ;; Remove all of the ugly *bars
  (when (functionp 'menu-bar-mode)
    (menu-bar-mode f))
  (when (functionp 'set-scroll-bar-mode)
    (set-scroll-bar-mode 'nil))
  (when (functionp 'mouse-wheel-mode)
    (mouse-wheel-mode f))
  (when (functionp 'tooltip-mode)
    (tooltip-mode f))
  (when (functionp 'tool-bar-mode)
    (tool-bar-mode f))
  (when (functionp 'blink-cursor-mode)
    (blink-cursor-mode f)))

;; Key Bindings
(global-set-key (kbd "C-s")      'isearch-forward-regexp)
(global-set-key (kbd "C-r")      'isearch-backward-regexp)
(global-set-key (kbd "M-%")      'query-replace-regexp)
(global-set-key (kbd "C-x C-l")  'toggle-truncate-lines)

;; Change this around for Dvorak
(define-key key-translation-map "\C-t" "\C-x")

;; Enable stock disabled commands
(put 'erase-buffer 'disabled nil)

;; If you actually enjoy writing yes/no, you should be beaten with a hose
(defalias 'yes-or-no-p 'y-or-n-p)

(defun my/add-watchwords ()
  "Highlight FIXME and TODO in code"
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\)\\>"
          1 '((:foreground "#DC8CC3")) t))))
(add-hook 'prog-mode-hook 'my/add-watchwords)

(defun my/c-mode-init()
  "Various style settings for C."
  (c-set-style              "k&r")
  (c-toggle-electric-state  f)
  (setq c-basic-offset      4))
(add-hook 'c-mode-hook #'my/c-mode-init)

;; Go
(defun my/go-mode-init()
  "Various style settings for Go."
  (setq c-basic-offset    8
        indent-tabs-mode  t))
(add-hook 'go-mode-hook #'my/go-mode-init)

;; XML
(defun my/nxml-mode-init()
  "Various style settings for nXML."
  (setq subword-mode                   t
        nxml-child-indent              4
        nxml-slash-auto-complete-flag  t))
(add-hook 'nxml-mode-hook #'my/nxml-mode-init)

;; General settings that should be applied to all programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (visual-line-mode  t)
            (subword-mode      t)))

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

;; Use this for linux
(when (eq system-type 'gnu/linux)
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

;; Stop putting temp files in the same directory, if I need it put it elsewhere
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(defun pretty-print-json ()
  "Pretty print JSON."
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool " (current-buffer) t)))

(defun pretty-print-xml ()
  "Pretty print XML."
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "xmllint --format -" (current-buffer) t))
  (indent-buffer))

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

;; packages
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

;; expand-region
(use-package expand-region
  :bind (("C-c e"   . er/expand-region)
         ("C-c C-e" . er/contract-region))
  :config
  (pending-delete-mode t))

;; hideshow
(use-package hideshow
  :bind
  ("C-c TAB" . hs-toggle-hiding)
  ("C-\\"    . hs-toggle-hiding)
  ("M-\\"    . hs-hide-all)
  ("M-|"     . hs-show-all)
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode t))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; company
(use-package company
  :init
  (setq company-idle-delay             0.1
        company-minimum-prefix-length  3
        company-selection-wrap-around  t
        company-dabbrev-downcase       nil
        company-transformers           '(company-sort-by-occurrence))
  (bind-keys :map company-active-map
             ("C-n"   . company-select-next)
             ("C-p"   . company-select-previous)
             ("C-d"   . company-show-doc-buffer)
             ("<tab>" . company-complete))
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
          popwin:special-display-config)))

;; git-gutter
(use-package git-gutter
  :idle
  (git-gutter-mode t)
  :bind
  (("C-x =" . git-gutter:popup-hunk)
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
  :mode "\\.js\\'"
  :config
  (setq-default js2-basic-offset 2))

;; dired
(use-package dired
  :bind
  ("C-x C-j" . dired-jump)
  :config
  (use-package dired-x
    :config
    (when (eq system-type 'darwin)
      (add-to-list 'dired-omit-extensions ".DS_Store")
      (setq insert-directory-program "/usr/local/bin/gls")
      (setq dired-listing-switches "-aBhl --group-directories-first"))
    (setq ls-lisp-dirs-first         t
          delete-by-moving-to-trash  t
          dired-dwim-target          t)
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
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C-c >"       . mc/mark-next-like-this)
   ("C-c <"       . mc/mark-previous-like-this)
   ("C-c C-a"     . mc/mark-all-like-this)))

;; helm
(use-package helm
  :bind
  (("C-x b"   . helm-mini) ; replace normal buffer window
   ("C-x k"   . helm-mini) ; replace kill buffer options
   ("M-x"     . helm-M-x)
   ("C-x C-i" . helm-semantic-or-imenu)
   ("M-y"     . helm-show-kill-ring)
   ("C-x f"   . helm-find-files)
   ("C-h b"   . helm-descbinds))
  :config
  (progn
    (use-package helm-files)
    (use-package helm-config)
    (setq helm-buffers-fuzzy-matching  t
          helm-truncate-lines          t)
    (use-package helm-swoop
      :bind
      (("M-i" . helm-swoop)
       ("M-I" . helm-multi-swoop)))
    (use-package helm-descbinds
      :init
      (helm-descbinds-mode t))))

;; eww
(use-package eww
  :bind
  (("C-c w" . eww)
   ("C-c o" . eww-browse-with-external-browser)))

;; ido
(use-package ido
  :init
  (ido-mode t)
  :config
  (progn
    (setq ido-use-virtual-buffers                 nil
          ido-enable-prefix                       nil
          ido-enable-flex-matching                t
          ido-auto-merge-work-directories-length  nil
          ido-create-new-buffer                   'always
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

;; org
(use-package org
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c b" . org-iswitchb)
   ("C-c l" . org-store-link))
  :config
  (progn
    (use-package ox-odt)
    
    ;; Setup org directories
    (setq org-directory "~/.org")

    ;; (setq org-agenda-files '("~/.org/personal.org"
    ;;                          "~/.org/work.org"
    ;;                          "~/.org/school.org"))

    ;; capture settings
    (setq org-capture-templates '(("t" "To do" entry (file "~/.org/refile.org")
                                   "* TODO %?\n%U\n")))
    (setq org-refile-targets    '((nil :maxlevel . 3)
                                  (org-agenda-files :maxlevel . 3)))
    (setq org-refile-use-outline-path             t
          org-refile-allow-creating-parent-nodes  'confirm)

    ;; todo settings
    (setq org-todo-keywords            '(("TODO(t)" "INPROGRESS(i)" "BLOCKED(b)" "DONE(d)"))
          org-use-fast-todo-selection  t
          org-refile-use-outline-path  t
          ido-everywhere               t)

    ;; miscellaneous settings
    (setq org-list-allow-alphabetical t)
    (setq org-src-fontify-natively t)

    ;; key mappings
    (define-key org-mode-map (kbd "C-c t") 'org-todo)

    (add-hook 'org-mode-hook (lambda ()
                               (set-fill-column 79)
                               (turn-on-auto-fill)))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((dot . t)
       (latex . t)))))

(use-package switch-window
  :bind
   ("C-c g" . switch-window))

(use-package jabber
  :config
  (setq jabber-roster-line-format "%c %-25n %u %-8s  %S"))

;; Graveyard, where old config goes to die.
;; smex
;; Trying out helm for now
;; (use-package smex
;;   :bind
;;   ("M-x" . smex)
;;   ("M-X" . smex-major-mode-commands)
;;   :config
;;   (setq smex-save-file (concat user-emacs-directory "misc/smex/.smex-items")))
