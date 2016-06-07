;; Package initialization 
(require 'package)
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
  :config
  ;; Redefine evil bindings to make more sense on Dvorak. I want to define my
  ;; own, as evil-dvorak didn't use the same keys I wanted to setup.
  (define-key evil-normal-state-map "d" 'evil-backward-char)
  (define-key evil-normal-state-map "h" 'evil-next-line)
  (define-key evil-normal-state-map "t" 'evil-previous-line)
  (define-key evil-normal-state-map "n" 'evil-forward-char)
  (define-key evil-normal-state-map "k" 'evil-delete)	    ; Think "kill".
  (define-key evil-normal-state-map "j" 'evil-find-char-to) ; Think "jump".
  (define-key evil-normal-state-map "J" 'evil-find-char-to-backward)
  (define-key evil-motion-state-map "l" 'evil-search-next)  ; Think "look".
  (define-key evil-motion-state-map "L" 'evil-search-previous)
  (define-key evil-normal-state-map "gj" 'evil-join)
  ;; Practically a requirement if you're using evil-mode. I like setting these
  ;; keys here, rather than with their respective packages, so I can easily find
  ;; confilcts, should they arise.
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
    (evil-leader/set-key-for-mode 'flycheck-mode
      "e" 'flycheck-next-error
      "E" 'flycheck-previous-error)
    (evil-leader/set-key
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
      "b"  'helm-buffers-list))
  ;; Expands matching delimiters to include quotes and more.
  (use-package evil-matchit
    :ensure t)
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

(let
    ((font (cond
            ((eq window-system 'w32) "Fantasque Sans Mono")
            (t "monospace"))))
  (set-frame-font (concat font " 12")))
(set-frame-font "Fantasque Sans Mono 12") ; "monospace" set in fonts.conf
(menu-bar-mode -1)                      ; Disable menu bar.
(scroll-bar-mode -1)                    ; Disable scroll bar.
(tool-bar-mode -1)                      ; Disable tool bar.
(blink-cursor-mode -1)                  ; Disable blinking cursor.
(load-theme 'base16-eighties-dark t)	; Theme from base16-theme package

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages

(use-package avy			; Jump to characters.
  :ensure t
  :config
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?t ?n ?s))) ; Use Dvorak home row.

(use-package powerline			; Powerline for the emacs' mode line.
  ;; TODO Not a fan the evil theme this or evil-powerline provide. Make my own.
  :ensure t
  :config (powerline-default-theme))

(use-package helm-config		; Narrowing framework.
  ;; TODO Look into setting up helm-dabbrev, helm-moccur, helm-projectile, and
  ;; helm-grep. 
  :ensure helm
  :demand t
  :config
  (use-package helm
    :ensure t
    :bind (:map helm-map
                ("<tab>" . helm-execute-persistent-action))
    :config
    (setq helm-split-window-in-side-p t) ; Open helm in current window.
    (setq helm-M-x-fuzzy-match t)	 ; Fuzzy match in M-x.
    (setq helm-buffers-fuzzy-matching t) ; Fuzzy match in buffers list.
    (setq helm-apropos-fuzzy-match t))	 ; Enable fuzzy searchi in apropos.
  (use-package helm-descbinds		 ; Better way to lookup keys.
    :ensure t))

(use-package org
  :delight org-mode "org"		; I like lowercase.
  :config
  ;; Not a fan of the bindings evil-org provides, so here are my own.
  (defun shellhead/smart-org-insert ()
    "Creates a new heading if currently in a heading, creates a new list item 
     if in a list, or creates a newline if neither."
    (interactive)
    (cond
     ((org-at-heading-p) (org-insert-heading-respect-content))
     ((org-at-item-p) (org-insert-item))))

  (evil-define-key 'insert org-mode-map
    (kbd "C-o") 'shellhead/smart-org-insert)
  (add-hook 'org-mode-hook #'shellhead/turn-on-auto-fill) ; Turn on auto-fill-mode
  (setq org-hide-leading-stars t))		       ; Hide all but the last star.

(use-package flycheck			; Syntax checker.
  :ensure t
  :diminish flycheck-mode
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (base16-theme flycheck zenburn-theme yaml-mode use-package tao-theme spacegray-theme solarized-theme smooth-scrolling rainbow-delimiters powerline popwin json-mode js2-mode helm-projectile helm-descbinds helm-c-yasnippet git-gutter evil-surround evil-org evil-nerd-commenter evil-matchit evil-magit evil-escape evil-easymotion dired-narrow delight company base16))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
