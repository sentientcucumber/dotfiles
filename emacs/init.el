;;; init.el -- A masterpiece in progress.

;; Author: Michael Hunsinger <mike.hunsinger@gmail.com>

;;; Commentary:

;; I really just wanted the flycheck highlighting to stop.

;;; Code:
(require 'package)			; Setup packages
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")) ; Add MELPA packages. 
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil-mode

;; I've come to rely on evil-mode a fair amount, so it get's its own section.
(use-package evil
  :diminish undo-tree-mode
  :ensure t
  :demand
  :init
  (setq evil-find-skip-newlines t)      ; Don't restrict find to current line.
  :config
  ;; Redefine evil bindings to make more sense on Dvorak. I want to define my
  ;; own, as evil-dvorak didn't use the same keys I wanted to setup.
  (define-key evil-normal-state-map "d" 'evil-backward-char)
  (define-key evil-normal-state-map "h" 'evil-next-line)
  (define-key evil-normal-state-map "t" 'evil-previous-line)
  (define-key evil-visual-state-map "n" 'evil-forward-char)
  (define-key evil-visual-state-map "d" 'evil-backward-char)
  (define-key evil-visual-state-map "h" 'evil-next-line)
  (define-key evil-visual-state-map "t" 'evil-previous-line)
  (define-key evil-normal-state-map "n" 'evil-forward-char)
  (define-key evil-normal-state-map "k" 'evil-delete)
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
      ;; Org
      "oa" 'org-agenda
      "oc" 'org-capture
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
    ;; flycheck-mode specific bindings
    ;; FIXME figure out why this isn't working.
    (evil-leader/set-key-for-mode 'global-flycheck-mode
      "e"  'flycheck-next-error
      "E"  'flycheck-previous-error))
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

(defalias 'yes-or-no-p 'y-or-n-p)	; A more sane "yes or no" default.
(setq inhibit-startup-message t)	; Open to scratch buffer instead.
(setq initial-major-mode 'fundamental-mode) ; Use a fundamental-mode on startup.
(setq indent-tabs-mode nil)             ; Only use spaces, never tabs.
(setq vc-follow-symlinks t)		; Follow symbollic links.
(setq echo-keystrokes 0.1)              ; Echo keystroke immediately

;; Putting temporary autosave files in the same directory is annoying. Move them
;; to "$EMACS_USER_DIRECTORY/tmp". Per the advisory at
;; https://www.emacswiki.org/emacs/AutoSave, lets not put it in "/tmp", incase
;; the computer crashes.
(defconst emacs-tmp-directory (concat user-emacs-directory "tmp"))
(setq backup-directory-alist `((".*" . ,emacs-tmp-directory)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-directory t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance

(defun is-windows-p ()
  "Returns t if the current window-system is Windows, or nil."
  (eq window-system 'w32))
    
;; I run emacs on a Windows machine at work, where none of my X config works
;; (thus "monospace" is not defined). I really want to use the X value where
;; possible, so whenever I have a change of heart and want to try a new
;; monospace font, I don't have to change it everywhere.
(let
    ((font (cond
            ((is-windows-p) "Fantasque Sans Mono")
            (t "monospace"))))
  (set-frame-font (concat font " 12")))

(menu-bar-mode -1)                      ; Disable menu bar.
(scroll-bar-mode -1)                    ; Disable scroll bar.
(tool-bar-mode -1)                      ; Disable tool bar.
(blink-cursor-mode -1)                  ; Disable blinking cursor.
(delight 'emacs-lisp-mode "emacs-lisp" :major) ; Me being neurotic.

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

(defun my/add-watchwords ()
  "Highlight FIXME, TODO, and NOCOMMIT in code TODO"
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME:?\\)\\>"
	  1 '((:foreground "#EF7373") (:slant italic)) t)
	 ("\\<\\(TODO:?\\)\\>"
          1 '((:foreground "#FFCC80") (:slant italic)) t))))
(add-hook 'prog-mode-hook #'my/add-watchwords)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages

(use-package avy			; Jump to characters.
  :ensure t
  :config
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?t ?n ?s))) ; Use Dvorak home row.

(use-package helm-config		; Narrowing framework.
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
    (setq helm-split-window-in-side-p t) ; Open helm in current window.
    (setq helm-M-x-fuzzy-match t)	 ; Fuzzy match in M-x.
    (setq helm-buffers-fuzzy-matching t) ; Fuzzy match in buffers list.
    (setq helm-apropos-fuzzy-match t))	 ; Enable fuzzy searchi in apropos.
  (use-package helm-descbinds		 ; Better way to lookup keys.
    :ensure t))

(use-package org
  :delight org-mode "org"               ; I like lowercase.
  :config
  (setq org-catch-invisible-edits 'show)    ; Prevent editing folded sections.

  ;; Not a fan of the bindings evil-org provides, so here are my own.
  ;; TODO create an additional state, org, that can be entered from INSERT
  ;; mode. This mode should then have its own keymap devoted to various org
  ;; functions (there are so many).
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
  (add-hook 'org-mode-hook #'shellhead/turn-on-auto-fill) ; Turn on auto-fill-mode
  (setq org-hide-leading-stars t))		       ; Hide all but the last star.

(use-package flycheck			; Syntax checker.
  :ensure t
  :diminish flycheck-mode
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package company			; Autocompletion.
  :ensure t
  :diminish company-mode
  :bind (:map company-active-map	; Remap active keymap.
              ("C-h" . company-select-next)
              ("C-t" . company-select-previous))
  :init (add-hook 'prog-mode-hook #'company-mode)
  :config
  (setq company-minimum-prefix-length 5) ; Look for completions after 5 chars.
  (setq company-idle-delay 0.25))	 ; Look for completions after 0.25s.

(use-package smooth-scrolling		; Make scrolling MUCH smoother.
  :ensure t
  :init (add-hook 'after-init-hook #'smooth-scrolling-mode))

;; Setup powerline theme for emacs, offers much better evil integration than the
;; powerline and powerline-evil packages.
(use-package telephone-line
  :ensure t
  :config
  (setq telephone-line-lhs
        '((evil . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-major-mode-segment
		     telephone-line-minor-mode-segment))
	  (nil . (telephone-line-vc-segment))))
  (setq telephone-line-rhs
	'((nil . (telephone-line-airline-position-segment))
	  (accent . (telephone-line-buffer-segment))))
  (telephone-line-mode t))

(use-package apropospriate-theme
  :ensure t
  :config (load-theme 'apropospriate-dark t))
