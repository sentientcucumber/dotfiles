;;; init.el -- A masterpiece in progress.

;; Author: Michael Hunsinger <mike.hunsinger@gmail.com>

;;; Commentary:

;; I really just wanted the flycheck highlighting to stop.

;;; Code:
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil-mode

;; I've come to rely on evil-mode a fair amount, so it get's its own section.
(use-package evil
  :diminish undo-tree-mode
  :ensure t
  :demand
  :init
  (setq evil-find-skip-newlines t)
  :config
  ;; Redefine evil bindings to make more sense on Dvorak. I want to define my
  ;; own, as evil-dvorak didn't use the same keys I wanted to setup.
  (define-key evil-normal-state-map "d" 'evil-backward-char) ;
  (define-key evil-normal-state-map "h" 'evil-next-line)
  (define-key evil-normal-state-map "t" 'evil-previous-line)
  (define-key evil-visual-state-map "n" 'evil-forward-char)
  (define-key evil-visual-state-map "d" 'evil-backward-char)
  (define-key evil-visual-state-map "h" 'evil-next-line)
  (define-key evil-visual-state-map "t" 'evil-previous-line)
  (define-key evil-normal-state-map "n" 'evil-forward-char)
  (define-key evil-normal-state-map "k" 'evil-delete)
  (define-key evil-visual-state-map "k" 'evil-delete)
  (define-key evil-normal-state-map "K" 'evil-delete-line)
  (define-key evil-normal-state-map "j" 'evil-find-char-to)
  (define-key evil-normal-state-map "J" 'evil-find-char-to-backward)
  (define-key evil-motion-state-map "l" 'evil-search-next)
  (define-key evil-motion-state-map "L" 'evil-search-previous)
  (define-key evil-normal-state-map "gj" 'evil-join)
  ;; Practically a requirement if you're using evil-mode. I like setting these
  ;; keys here, rather than with their respective packages, so I can easily find
  ;; confilcts, should they arise.
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
      ;; Org, while a major mode, I want these available all the time
      "oa" 'org-agenda
      "oc" 'org-capture
      ;; Dired
      "d"  'dired-jump
      ;; Commenting
      "ci" 'evilnc-comment-or-uncomment-lines
      "cp" 'evilnc-comment-or-uncomment-paragraphs
      "cr" 'comment-or-uncomment-region
      "cl" 'comment-indent
      ;; Helm
      "a"  'helm-apropos
      "k"  'helm-descbinds
      "m"  'helm-man-woman
      "x"  'helm-M-x
      "f"  'helm-find-files
      "b"  'helm-buffers-list)
    (evil-leader/set-key-for-mode 'org-mode
      "cp" 'org-set-property))
  ;; Expands matching delimiters to include quotes and more.
  (use-package evil-matchit
    :ensure t
    :init (global-evil-matchit-mode 1))
  ;; Makes moving around in evil so much easier by using avy.
  (use-package evil-easymotion
    :ensure t
    :config
    ;; The changes made to evil-mode do not carry over to evil-easymotion.
    (evilem-default-keybindings "SPC")
    (evilem-define (kbd "SPC h") 'evil-next-line)
    (evilem-define (kbd "SPC t") 'evil-previous-line)
    (evilem-define (kbd "SPC j") 'evil-find-char-to)
    (evilem-define (kbd "SPC J") 'evil-find-char-to-backward)
    (evilem-define (kbd "SPC k") nil))
  ;; Package for better commenting in evil.
  (use-package evil-nerd-commenter
    :ensure t)
  (evil-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings

(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default default-tab-width 2 
              indent-tabs-mode nil)
(setq inhibit-startup-message t
      initial-major-mode 'fundamental-mode
      vc-follow-symlinks t
      echo-keystrokes 0.1
      auto-save-default nil)

;; Putting temporary autosave files in the same directory is annoying. Move them
;; to "$EMACS_USER_DIRECTORY/tmp". Per the advisory at
;; https://www.emacswiki.org/emacs/AutoSave, lets not put it in "/tmp", incase
;; the computer crashes.
(defconst emacs-tmp-directory (concat user-emacs-directory "tmp"))
(setq backup-directory-alist `((".*" . ,emacs-tmp-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance

(defun is-windows-p ()
  "Returns t if the current window-system is Windows, or nil."
  (eq window-system 'w32))
    
;; I run emacs on a Windows machine at work, where none of my X config works
;; (thus "monospace" is not defined). I really want to use the X value where
;; possible, so whenever I have a change of heart and want to try a new
;; monospace font, I don't have to change it everywhere.
(let ((font (cond ((is-windows-p) "Fantasque Sans Mono")
                  (t "monospace"))))
  (set-frame-font (concat font " 11")))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(load-theme 'gruvbox t)

(delight 'emacs-lisp-mode "emacs-lisp" :major)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks

;; Some help from https://www.emacswiki.org/emacs/DelightedModes on getting this
;; to work with auto-fill-mode.
(defun shellhead/turn-on-auto-fill ()
  "Enable auto-fill-mode, set fill-column to 80, and apply only to comments."
  (auto-fill-mode 1)
  (set-fill-column 80)
  (setq comment-auto-fill-only-comments t)
  (delight 'auto-fill-function nil t))
(add-hook 'prog-mode-hook #'shellhead/turn-on-auto-fill)
(add-hook 'conf-mode-hook #'shellhead/turn-on-auto-fill)

(defun shellhead/add-watchwords ()
  "Highlight FIXME, TODO, and NOTE."
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME:?\\)\\>"
          1 '((:foreground "#fb4934") (:slant italic)) t)
         ("\\<\\(TODO:?\\)\\>"
          1 '((:foreground "#fabd2f") (:slant italic)) t)
         ("\\<\\(NOTE:?\\)\\>"
          1 '((:foreground "#8ec07c") (:slant italic)) t))))
(add-hook 'prog-mode-hook #'shellhead/add-watchwords)
(add-hook 'conf-mode-hook #'shellhead/add-watchwords)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages

(use-package avy
  ;; evil-easymotion uses avy.
  :ensure t
  :config
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?t ?n ?s)))

(use-package helm-config
  ;; TODO Look into setting up helm-dabbrev, helm-moccur, helm-projectile, and
  ;; helm-grep. 
  :ensure helm
  :demand t
  :config
  (use-package helm
    :ensure t
    :bind (:map helm-map
		("C-z" . helm-select-action)
                ("<tab>" . helm-execute-persistent-action))
    :config
    (setq helm-split-window-in-side-p t
          helm-M-x-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-apropos-fuzzy-match t))
  (use-package helm-descbinds
    :ensure t))

(use-package org
  :delight org-mode "org"
  :config
  (setq org-catch-invisible-edits 'show
        org-hide-leading-stars t
        org-use-property-inheritance t)

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

  (evil-define-key 'insert org-mode-map
    (kbd "C-o") 'shellhead/smart-org-insert)

  (add-hook 'org-mode-hook #'shellhead/turn-on-auto-fill))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package company
  :ensure t
  :diminish company-mode
  :bind (:map company-active-map	; Remap active keymap.
              ("C-h" . company-select-next)
              ("C-t" . company-select-previous))
  :init (add-hook 'prog-mode-hook #'company-mode)
  :config
  (setq company-minimum-prefix-length 4 
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
              ("/" . dired-narrow))
  :config
  (use-package dired-narrow
    :ensure t)
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-listing-switches "-ahl"
        dired-recursive-copies 'always
        ls-lisp-dirs-first t
        ls-lisp-ignore-case t))
;;; init.el ends here
