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
  ;; Practically a requirement if you're using evil-mode.
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      ;; Commenting
      "ci" 'evilnc-comment-or-uncomment-lines
      "cp" 'evilnc-comment-or-uncomment-paragraphs
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
    (evilem-default-keybindings "SPC"))
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

(set-frame-font "monospace 12")         ; "monospace" set in fonts.conf
(menu-bar-mode -1)                      ; Disable menu bar.
(scroll-bar-mode -1)                    ; Disable scroll bar.
(tool-bar-mode -1)                      ; Disable tool bar.
(blink-cursor-mode -1)                  ; Disable blinking cursor.
(load-theme 'base16-eighties-dark t)	; Theme from base16-theme package

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks

;; Some help from https://www.emacswiki.org/emacs/DelightedModes on getting this
;; to work with auto-fill-mode.
(defun shellhead/auto-fill-hook ()
  "Setup auto-fill-mode by setting the fill-column, and only apply to comments,
diminish (or delight) the mode."
  (auto-fill-mode 1)
  (set-fill-column 80)
  (setq comment-auto-fill-only-comments t)
  (delight 'auto-fill-function nil t))

(add-hook 'prog-mode-hook #'shellhead/auto-fill-hook)

(defun shellhead/company-hook ()
  (company-mode 1))

(add-hook 'prog-mode-hook #'shellhead/company-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages

;; Have avy use the Dvorak home row.
(use-package avy
  :ensure t
  :config (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?t ?n ?s)))

;; Powerline status line for emacs.
;; TODO Build out personal theme, this or powerline-evil don't provide
;; the themes I want.
(use-package powerline
  :ensure t
  :config (powerline-default-theme))

(use-package helm-config		; Narrowing framework.
  :ensure helm
  :demand t
  :config
  (use-package helm
    :ensure t
    :bind (:map helm-map
                ("<tab>" . helm-execute-persistent-action)))
  (setq helm-split-window-in-side-p t) ; Open helm in current window.
  (use-package helm-descbinds	       ; Better way to lookup keys.
    :ensure t))

(use-package org
  :delight org-mode "org"		; I like lowercase.
  :config
  (add-hook 'org-mode-hook #'shellhead/auto-fill-hook) ; Turn on auto-fill-mode
  (setq org-hide-leading-stars t))		       ; Hide all but the last star.

(use-package flycheck
  :ensure t)

(use-package company			; Autocompletion.
  :ensure t
  :bind (:map company-active-map	; Remap active keymap.
              ("C-h" . company-select-next)
              ("C-t" . company-select-previous))
  :config
  (setq company-minimum-prefix-length 5) ; Look for completions after 5 chars.
  (setq company-idle-delay 0.25))	 ; Look for completions after 0.25s.
