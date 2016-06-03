;; Package initialization 
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
      "x"  'helm-M-x
      "f"  'helm-find-files))
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

;; Putting temporary autosave files in the same directory is annoying. Move them
;; to "$EMACS_USER_DIRECTORY/tmp". Per the advisory at
;; https://www.emacswiki.org/emacs/AutoSave, lets not put it in "/tmp", incase
;; the computer crashes.
(defconst emacs-tmp-directory (concat user-emacs-directory "tmp"))
(setq backup-directory-alist `((".*" . ,emacs-tmp-directory)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-directory t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings

(defalias 'yes-or-no-p 'y-or-n-p)	; A more sane "yes or no" default.
(setq inhibit-startup-message t)	; Open to scratch buffer instead.
(setq indent-tabs-mode nil)             ; Only use spaces, never tabs.
(setq vc-follow-symlinks t)		; Follow symbollic links.
(setq echo-keystrokes 0.1)              ; Echo keystroke immediately

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance

;; "monospace" is defined outside of emacs, in .config/fontconfig/font.conf
(set-frame-font "monospace 12")
(menu-bar-mode -1)                      ; Disable menu bar.
(scroll-bar-mode -1)                    ; Disable scroll bar.
(tool-bar-mode -1)                      ; Disable tool bar.
(blink-cursor-mode -1)                  ; Disable blinking cursor.
(load-theme 'base16-eighties-dark t)	; Theme from base16-theme package

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks

(defun shellhead/auto-fill-hook ()
  "Setup auto-fill-mode by setting the fill-column, and only apply to comments,
diminish (or delight) the mode. Some help from 
https://www.emacswiki.org/emacs/DelightedModes on getting this to work with
auto-fill-mode."
  (set-fill-column 80)
  (setq comment-auto-fill-only-comments t)
  (delight 'auto-fill-function nil t))
(add-hook 'prog-mode-hook #'shellhead/auto-fill-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages

;; Have avy use the Dvorak home row.
(use-package avy
  :ensure t
  :config (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?t ?n ?s)))

;; Powerline status line for emacs.
;; TODO Build out personal theme, this or powerline-evil don't provide
;; the themes I want.
(use-package powerline
  :config (powerline-default-theme))

(use-package helm-config
  :ensure helm
  :demand t
  :config
  (setq helm-split-window-in-side-p t))  ; Open helm in current window
