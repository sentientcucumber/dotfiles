;;; init.el -- A masterpiece in progress.

;; Author: Michael Hunsinger <mike.hunsinger@gmail.com>

;;; Commentary:

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil-mode

(use-package evil
  :demand
  :diminish undo-tree-mode
  :ensure t
  :config
  ;; motion state
  (define-key evil-motion-state-map "L" 'evil-search-previous)
  (define-key evil-motion-state-map "l" 'evil-search-next)
  (define-key evil-normal-state-map "d" 'evil-backward-char)
  (define-key evil-normal-state-map "h" 'evil-next-visual-line)
  (define-key evil-normal-state-map "j" 'evil-find-char-to)
  (define-key evil-normal-state-map "k" 'evil-delete)
  (define-key evil-normal-state-map "n" 'evil-forward-char)
  (define-key evil-normal-state-map "t" 'evil-previous-visual-line)
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
      ;; flycheck
      "e"  'flycheck-next-error
      "E"  'flycheck-previous-error
      ;; org
      "oa" 'org-agenda
      "oc" 'org-capture
      ;; dired
      "d"  'dired-jump
      ;; commenting
      "cl" 'evilnc-comment-or-uncomment-lines
      "cp" 'evilnc-comment-or-uncomment-paragraphs
      "cr" 'comment-or-uncomment-region
      "ci" 'comment-indent
      ;; ivy
      "b"  'ivy-switch-buffer
      ;; projectile
      "F"  'counsel-projectile-find-file
      ;; counsel
      "x"  'counsel-M-x
      "f"  'counsel-find-file
      "/"  'counsel-grep-or-swiper
      "cg" 'counsel-git-grep
      "cd" 'counsel-dired-jump
      "hf" 'counsel-describe-function
      "hv" 'counsel-describe-variable
      "hk" 'counsel-descbinds
      ;; paredit
      "P"  'hydra-paredit-menu/body
      ;; move-text
      "M"  'hydra-move-text-menu/body
      ;; imenu
      "I"  'ivy-imenu-anywhere)
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
(setq-default default-tab-width 4
              indent-tabs-mode nil
              fill-column 80
              ring-bell-function (lambda ())
              abbrev-mode t)

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

(set-frame-font "Fira Code 11")
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(diminish 'abbrev-mode)

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks

(defun my/turn-on-auto-fill ()
  "Enable `auto-fill-mode', set `fill-column' to 80, and apply only to comments."
  (auto-fill-mode 1)
  (setq comment-auto-fill-only-comments t)
  (delight 'auto-fill-function nil t))

(defun my/highlight-watchwords ()
  "Make FIXME, TODO, and NOTE standout in buffers."
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME:?\\)\\>"
          1 '((:foreground "#fb4934") (:slant italic)) t)
         ("\\<\\(TODO:?\\)\\>"
          1 '((:foreground "#fabd2f") (:slant italic)) t)
         ("\\<\\(NOTE:?\\)\\>"
          1 '((:foreground "#8ec07c") (:slant italic)) t))))


(defun my/turn-on-electric-pairs ()
  "Enable variable `electric-pair-mode'."
  (electric-pair-mode 1)
  (setq electric-pair-preserve-balance t
        electric-pair-delete-adjacent-pairs t
        electric-pair-open-newline-between-pairs t)
  (show-paren-mode 1))

(defun my/nxml-mode-hook ()
  "Settings for `nxml-mode'."
  (setq nxml-slash-auto-complete-flag t
        nxml-child-indent 4))

(defun my/java-mode-hook ()
  "Setting for `java-mode'."
  (c-set-style "java")
  (eclim-mode))

;; `prog-mode' hooks
(add-hook 'prog-mode-hook #'my/turn-on-auto-fill)
(add-hook 'prog-mode-hook #'my/turn-on-electric-pairs)
(add-hook 'prog-mode-hook #'my/highlight-watchwords)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'linum-mode)

;; mode specific hooks
(add-hook 'nxml-mode-hook #'my/nxml-mode-hook)
(add-hook 'java-mode-hook #'my/java-mode-hook)

;; miscellaneous
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages

(use-package eclim
  :ensure t
  :config
  (setq eclim-accepted-file-regexps '("\\.java$")
        eclim-use-yasnippet nil
        help-at-pt-display-when-idle t
        help-at-pt-timer-delay 0.1
        eclim-auto-save nil)
  (help-at-pt-set-timer)
  (use-package eclimd))

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

(use-package ivy
  :ensure t
  :demand
  :diminish ivy-mode
  ;; throw some Dvorak flavor in there
  :bind (("C-h" . ivy-next-line)
         ("C-t" . ivy-previous-line))
  :config
  (ivy-mode)
  (use-package counsel-projectile
    :ensure t
    :config
    (counsel-projectile-on)))

(use-package projectile
  :ensure t
  :diminish projectile-mode)

(use-package org
  :preface
  ;; TODO create an additional state, "org", that can be entered from INSERT
  ;; mode. This mode should then have its own keymap devoted to various org
  ;; functions (there are so many).

  ;; Not a fan of the bindings evil-org provides, so here are my own.
  ;; FIXME This doesn't work on multiline list entries.
  (defun my/smart-org-insert ()
    "Creates a new heading if currently in a heading, creates a new list item
if in a list, or creates a newline if neither."
    (interactive)
    (cond
     ((org-at-heading-p) (org-insert-heading-respect-content))
     ((org-at-item-p) (org-insert-item))
     (t (evil-open-below 1))))

  (defun my/org-mode-hook ()
    "Setup for org files."
    (org-indent-mode t)
    (visual-fill-column-mode t)
    (visual-line-mode)
    (eval-after-load 'org-indent '(diminish 'org-indent-mode))
    (diminish 'visual-line-mode))
  :config
  (add-hook 'org-mode-hook 'my/org-mode-hook)
  (setq org-catch-invisible-edits 'show
        org-hide-leading-stars t
        org-use-property-inheritance t)
  (evil-define-key 'insert org-mode-map
    (kbd "C-o") 'my/smart-org-insert)
  (modify-syntax-entry ?~ "(~" org-mode-syntax-table)
  (modify-syntax-entry ?= "(=" org-mode-syntax-table))

(use-package flycheck
  :ensure t
  :delight flycheck-mode
  :init (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package company
  :ensure t
  :diminish company-mode
  :bind (:map company-active-map
              ("C-h" . company-select-next)
              ("C-t" . company-select-previous))
  :config
  (setq company-minimum-prefix-length 3
                company-idle-delay 0.15)
  :init (add-hook 'after-init-hook #'global-company-mode))

(use-package smooth-scrolling
  :ensure t
  :init (add-hook 'after-init-hook #'smooth-scrolling-mode))

(use-package tramp
  :config
  (setq tramp-default-method "ssh"
        tramp-use-ssh-controlmaster-options nil))

(use-package dired
  :bind (:map dired-mode-map
              ("RET" . dired-find-alternate-file)
              ("/" . dired-narrow)
              ("C-u" . dired-up-directory)
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
  ;; requires the aspell and aspell-en packages
  :if (executable-find "aspell")
  :diminish flyspell-mode
  :init
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (add-hook 'org-mode-hook #'flyspell-mode-on)
  :config
  (setq ispell-program-name (executable-find "aspell"))
  (setq ispell-extra-args '("--sug-mode=fast"
                            "--lang=en_US"
                            "--ignore=4")))

(use-package lisp-mode
  :preface
  (defun my/elisp-mode-hook ()
    "Settings for `emacs-lisp' modes"
    (eldoc-mode 1))
  :config
  (add-hook 'emacs-lisp-mode-hook 'my/elisp-mode-hook))

(use-package paredit-everywhere
  :ensure t
  :commands (paredit-forward-slurp-sexp paredit-forward-barf-sexp)
  :config
  (paredit-everywhere-mode t))

(use-package move-text
  :ensure t)

(use-package imenu-anywhere
  :ensure t
  :config
  (use-package imenu+
    :config
    (setq imenup-sort-ignores-case-flag t
          imenup-ignore-comments-flag t))
  (setq imenu-sort-function 'imenu--sort-by-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Skeletons

(define-skeleton skeleton/maven-dependency
  "Create a Maven dependency chunk."
  > "<dependency>" \n
  > "<groupId>" (skeleton-read "group id: ") "</groupId>" \n
  > "<artifactId>" (skeleton-read "artifact id: ") "</artifactId>" \n
  > "<version>" (skeleton-read "version: ") "</version>" \n
  -4 "</dependency>")

(define-skeleton skeleton/java-class
  "Create a Java class based on the buffer name."
  > "package " (skeleton-read "package: ") ";" \n
  > \n
  > "public class " (substring (buffer-name) 0 -5) " {" \n
  > \n
  > _ \n
  > \n
  -4 "}" \n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hydras

(defhydra hydra-paredit-menu nil
  "paredit"
  ("s" paredit-forward-slurp-sexp "slurp forward")
  ("b" paredit-forward-barf-sexp "barf forward"))

(defhydra hydra-move-text-menu nil
  "move text"
  ("h" move-text-down "move text down")
  ("t" move-text-up "move text up"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Functions

(defun my/format-buffer ()
  "Format the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max)))

;;; init.el ends here)
