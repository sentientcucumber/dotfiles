;;; init.el -- A masterpiece in progress.

;; Author: Michael Hunsinger <mike.hunsinger@gmail.com>

;;; Commentary:

;; `server-mode'

;; It's best to run Emacs as a server, and then use emacsclient to connect.  I
;; grabbed this systemd script from
;; https://wiki.archlinux.org/index.php/Emacs#As_a_systemd_unit to run it as a
;; service.

;;     [Unit]
;;     Description=Emacs: the extensible, self-documenting text editor
;;     [Service]
;;     Type=forking
;;     ExecStart=/usr/bin/emacs --daemon
;;     ExecStop=/usr/bin/emacsclient --eval "(kill-emacs)"
;;     Restart=always
;;     [Install]
;;     WantedBy=default.target

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil-mode

(use-package evil
  :diminish undo-tree-mode
  :ensure t
  :demand
  :config
  ;; motion state
  (define-key evil-motion-state-map "L" 'evil-search-previous)
  (define-key evil-motion-state-map "l" 'evil-search-next)
  ;; normal state
  (define-key evil-normal-state-map "J" 'evil-find-char-to-backward)
  (define-key evil-normal-state-map "K" 'evil-delete-line)
  (define-key evil-normal-state-map "d" 'evil-backward-char)
  (define-key evil-normal-state-map "gj" 'evil-join)
  (define-key evil-normal-state-map "h" 'evil-next-visual-line)
  (define-key evil-normal-state-map "j" 'evil-find-char-to)
  (define-key evil-normal-state-map "k" 'evil-delete)
  (define-key evil-normal-state-map "n" 'evil-forward-char)
  (define-key evil-normal-state-map "t" 'evil-previous-visual-line)
  ;; visual state
  (define-key evil-visual-state-map "J" 'evil-find-char-to-backward)
  (define-key evil-visual-state-map "d" 'evil-backward-char)
  (define-key evil-visual-state-map "h" 'evil-next-visual-line)
  (define-key evil-visual-state-map "j" 'evil-find-char-to)
  (define-key evil-visual-state-map "k" 'evil-delete)
  (define-key evil-visual-state-map "n" 'evil-forward-char)
  (define-key evil-visual-state-map "t" 'evil-previous-visual-line)
  (use-package evil-leader
    :ensure t
    :init (global-evil-leader-mode)
    :config
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      ;; NOTE evil-leader/set-key-for-mode only works for major modes
      ;; Flycheck
      "e"  'flycheck-next-error
      "E"  'flycheck-previous-error
      ;; Org
      "oa" 'org-agenda
      "oc" 'org-capture
      ;; Dired
      "d"  'dired-jump
      ;; Commenting
      "cl" 'evilnc-comment-or-uncomment-lines
      "cp" 'evilnc-comment-or-uncomment-paragraphs
      "cr" 'comment-or-uncomment-region
      "ci" 'comment-indent
      ;; Helm
      "x"  'helm-M-x
      "f"  'helm-find-files
      "b"  'helm-buffers-list
      "ha"  'helm-apropos
      "hk"  'helm-descbinds
      "hm"  'helm-man-woman
      "hy"  'helm-show-kill-ring
      "hp"  'helm-projectile
      "hg"  'helm-projectile-grep
      "hf"  'helm-projectile-find-file
      "hs"  'helm-swoop
      "hS"  'helm-multi-swoop)
    (evil-leader/set-key-for-mode 'org-mode
      "cp" 'org-set-property)
    (evil-leader/set-key-for-mode 'dired-mode
      "ch" 'wdired-change-to-wdired-mode
      "i"  'dired-subtree-insert
      "k"  'dired-subtree-remove)
    (evil-leader/set-key-for-mode 'python-mode
      "va" 'venv-workon
      "vd" 'venv-deactivate))
  (use-package evil-matchit
    :ensure t
    :init (global-evil-matchit-mode 1))
  (use-package evil-easymotion
    :ensure t
    :config
    ;; The changes made to evil-mode do not carry over to evil-easymotion.
    (evilem-default-keybindings "SPC")
    (evilem-define (kbd "SPC h") 'evil-next-visual-line)
    (evilem-define (kbd "SPC t") 'evil-previous-visual-line)
    (evilem-define (kbd "SPC j") 'evil-find-char-to)
    (evilem-define (kbd "SPC J") 'evil-find-char-to-backward)
    (evilem-define (kbd "SPC k") nil))
  (use-package evil-nerd-commenter
    :ensure t)
  (use-package evil-surround
    :ensure t)
  (evil-mode t)
  (global-evil-surround-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings

(defalias 'yes-or-no-p 'y-or-n-p)

;; default settings
(setq-default default-tab-width 2 
              indent-tabs-mode nil
              fill-column 80
              ring-bell-function (lambda ()))

;; per buffer settings
(setq inhibit-startup-message t
      initial-major-mode 'fundamental-mode
      vc-follow-symlinks t
      echo-keystrokes 0.1
      auto-save-default nil
      make-backup-files nil
      column-number-mode t)

(put 'erase-buffer 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance

(set-frame-font "Hack 11")
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(load-theme 'gruvbox t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks

(defun shellhead/turn-on-auto-fill ()
  "Enable `auto-fill-mode', set `fill-column' to 80, and apply only to comments."
  (auto-fill-mode 1)
  (setq comment-auto-fill-only-comments t)
(delight 'auto-fill-function nil t))

(defun shellhead/highlight-watchwords ()
  "Highlight FIXME, TODO, and NOTE."
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME:?\\)\\>"
          1 '((:foreground "#fb4934") (:slant italic)) t)
         ("\\<\\(TODO:?\\)\\>"
          1 '((:foreground "#fabd2f") (:slant italic)) t)
         ("\\<\\(NOTE:?\\)\\>"
          1 '((:foreground "#8ec07c") (:slant italic)) t))))


(defun shellhead/turn-on-electric-pairs ()
  "Enable variable `electric-pair-mode'."
  (electric-pair-mode 1)
  (setq electric-pair-preserve-balance t
        electric-pair-delete-adjacent-pairs t
        electric-pair-open-newline-between-pairs t)
  (show-paren-mode 1))

;; prog-mode hooks
(add-hook 'prog-mode-hook #'shellhead/turn-on-auto-fill)
(add-hook 'prog-mode-hook #'shellhead/turn-on-electric-pairs)
(add-hook 'prog-mode-hook #'shellhead/highlight-watchwords)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages

(use-package python
  ;; TODO checkout cinspect, pytest/py-test
  :config
  (use-package virtualenvwrapper
    :ensure t
    :config
    (venv-initialize-interactive-shells)
    (venv-initialize-eshell)
    (setq venv-location "~/.venvs")))


(use-package avy
  :ensure t
  :config
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))


(use-package helm-config
  ;; TODO Look into setting up helm-dabbrev, helm-moccur, and
  ;; helm-grep. 
  :ensure helm
  :demand t
  :config
  (use-package helm
    :ensure t
    :bind (:map helm-map
                ("C-z" . helm-select-action)
                ("<tab>" . helm-execute-persistent-action)
                ;; get a little vim-esque flavor in there
                ("C-h" . helm-next-line)
                ("C-t" . helm-previous-line)
                ("C-k" . helm-buffer-run-kill-buffers))
    :config
    (setq helm-split-window-in-side-p t
          helm-M-x-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-apropos-fuzzy-match t))
  (use-package helm-descbinds
    :ensure t)
  (use-package helm-projectile 
    :ensure t
    :init
    (helm-projectile-on))
  (use-package helm-swoop
    :ensure t
    :config
    (setq helm-multi-swoop-edit-save t
          helm-swoop-split-with-multiple-windows t
          helm-swoop-split-direction 'split-window-vertically
          helm-swoop-speed-or-color t))
  (helm-autoresize-mode t)
  (setq helm-autoresize-min-height 30))


(use-package visual-fill-column
  :ensure t)


(use-package org
  :delight org-mode "org"
  :preface
  ;; TODO create an additional state, "org", that can be entered from INSERT
  ;; mode. This mode should then have its own keymap devoted to various org
  ;; functions (there are so many).

  ;; Not a fan of the bindings evil-org provides, so here are my own.
  ;; FIXME This doesn't work on multiline list entries.
  (defun shellhead/smart-org-insert ()
    "Creates a new heading if currently in a heading, creates a new list item 
     if in a list, or creates a newline if neither." 
    (interactive)
    (cond
     ((org-at-heading-p) (org-insert-heading-respect-content))
     ((org-at-item-p) (org-insert-item))
     (t (evil-open-below 1))))

  (defun shellhead/org-mode-hook ()
    "Setup for org files."
    (org-indent-mode t)
    (visual-fill-column-mode t)
    (visual-line-mode)
    (eval-after-load 'org-indent '(diminish 'org-indent-mode))
    (diminish 'visual-line-mode))
  :config
  (add-hook 'org-mode-hook 'shellhead/org-mode-hook)
  (setq org-catch-invisible-edits 'show
        org-hide-leading-stars t
        org-use-property-inheritance t)
  (evil-define-key 'insert org-mode-map
    (kbd "C-o") 'shellhead/smart-org-insert))


(use-package flycheck
  :ensure t
  :delight flycheck-mode
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode))


(use-package company
  :ensure t
  :diminish company-mode
  :bind (:map company-active-map
              ("C-h" . company-select-next)
              ("C-t" . company-select-previous))
  :init (add-hook 'prog-mode-hook #'company-mode)
  :config
  (setq company-minimum-prefix-length 3 
        company-idle-delay 0.15))


(use-package smooth-scrolling
  :ensure t
  :init (add-hook 'after-init-hook #'smooth-scrolling-mode))


(use-package tramp
  :config
  (setq tramp-default-method "ssh"
        tramp-use-ssh-controlmaster-options nil))


(use-package gruvbox-theme
  :ensure t)


(use-package dired
  :bind (:map dired-mode-map
              ("RET" . dired-find-alternate-file)
              ("/" . dired-narrow)
              ("C-h" . dired-next-line)
              ("C-t" . dired-previous-line))
  :config
  (use-package dired-narrow
    :ensure t)
  (use-package dired-filter
    :ensure t)
  (use-package dired-subtree
    :ensure t)
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-listing-switches "-ahl"
        dired-recursive-copies 'always
        ls-lisp-dirs-first t
        ls-lisp-ignore-case t
        directory-free-space-args "-Pkh"))


(use-package eldoc
  :diminish eldoc-mode)


(use-package flyspell
  ;; Dependencies for aspell and aspell-en 
  :ensure helm-flyspell
  :if (executable-find "aspell")
  :diminish flyspell-mode
  :bind (:map flyspell-mode-map
              ("C-;" . helm-flyspell-correct))
  :init (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :config
  (setq ispell-program-name (executable-find "aspell"))
  (setq ispell-extra-args '("--sug-mode=fast"
                            "--lang=en_US"
                            "--ignore=4")))


(use-package ido
  :ensure t
  :init (ido-mode 1)
  (use-package ido-ubiquitous
    :ensure t
    :init (ido-ubiquitous 1))
  (use-package ido-vertical-mode
    :ensure t
    :init
    (ido-vertical-mode 1)
    (setq ido-vertical-pad-list nil
          ido-vertical-show-count t)))


(use-package lisp-mode
  :preface
  (defun shellhead/elisp-mode-hook ()
    "Settings for `emacs-lisp' modes"
    (eldoc-mode 1))
  :config
  (add-hook 'emacs-lisp-mode-hook 'shellhead/elisp-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Skeletons

(define-skeleton python/try-except
  "Create a try/except block."
  "Exception: "
  > "try:" \n
  > _ \n
  -4 "except " str ":" \n)
;; init.el ends here
