;;; init.el --- My configuration.
;; 
;;; Commentary:
;; 
;; My Emacs configuration.
;; 
;; -*- coding: utf-8; lexical-binding: t -*-
;; 
;;; Code:

(package-initialize)
(require 'cask (concat
                (getenv "HOME")
                (if (eq window-system 'w32)
                    "\\.cask\\cask.el"
                  "/.cask/cask.el")))
(cask-initialize)

(defconst geek/dvorak-home-row '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s ?-)
  "Dvorak's home row.")

(defalias 'yes-or-no-p 'y-or-n-p
  "Use 'y' and 'n' to answer yes/no questions, like a sane human being.")

(setq-default indent-tabs-mode nil
              tab-always-indent t
              tab-width 4
              vc-follow-symlinks t
              mode-line-default-help-echo nil
              cursor-in-non-selected-windows nil
              use-file-dialog nil)

(windmove-default-keybindings 'meta)

(setq auto-save-default nil
      make-backup-files nil
      inhibit-startup-screen t
      initial-major-mode 'fundamental-mode)

(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (blink-cursor-mode -1)
  (column-number-mode 1)
  (set-frame-font "Fira Mono 11"))

(use-package zerodark-theme
  ;; I'd write some sort of witty remark about how I like this theme but I
  ;; change this every month.
  :init (load-theme 'zerodark t))

(use-package shackle
  :init
  (setq shackle-rules '(("*Help*" :select t :size 20 :align 'below)
                        ("*Warnings*" :select t :size 20 :align 'below)))
  (shackle-mode t))

(use-package smooth-scrolling
  :init (smooth-scrolling-mode t))

(use-package idle-highlight-mode
  :init (add-hook 'prog-mode-hook #'idle-highlight-mode)
  :config (setq idle-highlight-idle-time 0.5))

(use-package company
  :bind (("C-SPC" . company-complete)
         :map company-active-map
         ("C-h" . company-select-next)
         ("C-t" . company-select-previous))
  :init (setq company-show-numbers t
              company-idle-delay 0.1
              company-auto-complete nil))

(use-package nlinum
  :config (set-face-attribute 'linum nil :height 0.85 :slant 'normal))

(use-package highlight-indent-guides
  :config (setq highlight-indent-guides-method 'character))

(use-package ivy :demand
  ;; `ivy' is a completion framework, similar to `helm'.
  :bind (:map ivy-minibuffer-map
              ("C-h" . ivy-next-line)
              ("C-t" . ivy-previous-line))
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t
        ivy-fixed-height-minibuffer t
        ivy-count-format "%d/%d "
        ivy-height 10
        ivy-wrap t
        ivy-extra-directories nil))

(use-package evil :demand
  ;; Bring modal editing to Emacs.
  :bind
  (:map (evil-normal-state-map evil-visual-state-map evil-motion-state-map)
        ("d" . evil-backward-char)
        ("n" . evil-forward-char)
        ("h" . evil-next-visual-line)
        ("t" . evil-previous-visual-line)
        ("j" . evil-snipe-X)
        ("k" . evil-delete)
        ("K" . evil-delete-line)
        ("/" . swiper))
  :init
  (setq evil-echo-state nil
        evil-want-C-u-scroll t
        evil-find-skip-newlines t
        evil-normal-state-tag "N"
        evil-insert-state-tag (all-the-icons-faicon "i-cursor"
                                                    :v-adjust 0.1
                                                    :height 0.65)
        evil-visual-state-tag (all-the-icons-faicon "eye"
                                                    :v-adjust 0.05
                                                    :height 0.85)
        evil-emacs-state-tag "E"
        evil-operator-state-tag "O"
        evil-motion-state-tag (all-the-icons-faicon "arrows-alt"
                                                    :v-adjust 0.05
                                                    :height 0.85)
        evil-replace-state-tag "R"
        evil-default-cursor (face-attribute 'cursor
                                            :background
                                            nil
                                            'default)
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-operator-state-cursor '(hbar . 2)
        evil-visual-state-cursor `(,(face-attribute 'shadow
                                                    :background
                                                    nil
                                                    'default)
                                   box)
        evil-emacs-state-cursor `(,(face-attribute 'font-lock-builtin-face
                                                   :foreground
                                                   nil
                                                   'default)
                                  bar))
  (evil-mode t))

(use-package evil-lispy
  :bind
  (:map evil-lispy-state-mode-map
        ("h" . special-lispy-down)
        ("n" . special-lispy-right)
        ("t" . special-lispy-up)
        ("d" . special-lispy-left)
        ("T" . special-lispy-teleport))
  :config (setq evil-lispy-state-tag "L"
                lispy-avy-keys geek/dvorak-home-row
                lispy-comment-use-single-semicolon t))

(use-package evil-multiedit
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-easymotion
  ;; Use `avy' to show where movements will take you.
  :config
  (setq evilem-keys geek/dvorak-home-row) ; Dvorak nonsense
  (evilem-define (kbd "g SPC h") 'evil-next-visual-line)
  (evilem-define (kbd "g SPC t") 'evil-previous-visual-line))

(use-package evil-snipe
  ;; Move around a line quickly.
  :config (setq evil-snipe-smart-case t
                evil-snipe-repeat-scope 'visible)
  :init
  (evil-snipe-mode t)
  (evil-snipe-override-mode t))

(use-package general
  ;; Similar to `evil-leader'.
  :config
  (general-evil-setup)
  (general-nmap :prefix "M-t"
                "t"  'counsel-M-x
                "g"  'counsel-git
                "f"  'counsel-find-file
                "d"  'dired-jump
                "b"  'ivy-switch-buffer
                ;; help functions
                "hk" 'counsel-descbinds
                "hv" 'counsel-describe-variable
                "hf" 'counsel-describe-function
                "hz" 'zeal-at-point))

(use-package beacon
  ;; Useful to find what point I'm at when bouncing around and between buffers.
  :init (beacon-mode t)
  :config (setq beacon-blink-when-window-changes t
                beacon-blink-when-buffer-changes t))

(use-package dired
  ;; A builtin package to interact with files/directories.
  :commands (dired-jump)
  :bind (:map dired-mode-map
              ("RET" . dired-find-alternate-file)
              ("/" . dired-narrow)
              ("i" . dired-subtree-insert)
              ("I" . dired-subtree-remove)
              ("h" . dired-next-line)
              ("t" . dired-previous-line)
              ("M" . dired-unmark)
              ("u" . dired-up-directory))
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (put 'dired-find-alternate-file 'disabled nil)
  :config
  (setq dired-listing-switches
        "-o --almost-all --human-readable --group-directories-first"))

(defun geek/nxml-mode-hook ()
  "Make dealing with XML more tolerable."
  (setq nxml-slash-auto-complete-flag t))

(add-hook 'nxml-mode-hook #'geek/nxml-mode-hook)

(defun geek/prog-mode-hook ()
  "Setup for all modes inherited from `prog-mode'."
  (company-mode t)
  (column-enforce-mode t)
  (hl-line-mode t)
  (nlinum-mode t)
  (show-paren-mode t)
  (evil-surround-mode t)
  (electric-pair-local-mode t)
  (highlight-indent-guides-mode t)
  (flycheck-mode t))

(add-hook 'prog-mode-hook #'geek/prog-mode-hook)

(defun geek/emacs-lisp-mode-hook ()
  "Setup for `emacs-lisp-mode'."
  (setq dash-enable-fontlock t)
  (eldoc-mode t)
  (evil-lispy-mode t)
  (highlight-quoted-mode t))

(add-hook 'emacs-lisp-mode-hook #'geek/emacs-lisp-mode-hook)

(add-hook 'java-mode-hook (lambda ()
                            (column-enforce-n 120)))

(use-package python
  :init
  (setq python-shell-interpreter "python3")
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook #'anaconda-eldoc-mode))

(use-package company-anaconda :after python
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package virtualenv
  :config
  (setq virtualenv-root "~/.venvs"))

(use-package eclim
  ;; Interacts with a headless Eclipse server to perform a lot of the same
  ;; functions in Eclipse. Great for identifying syntax errors, running builds,
  ;; and more.

  ;; This requires an external package by the same name (eclim).
  :preface
  (defun geek/eclim-after-save-hook ()
    "Update the syntax problems on screen after saving."
    (eclim-problems-highlight))
  (defhydra geek/hydra-eclim-menu ()
    "
^Eclim Operations^                  ^ ^
^^^---------------------------------------
_i_: import               _g_: getter
_d_: declaration          _s_: setter 
_r_: references           _c_: constructor
"
    ("i" eclim-java-import-organize)
    ("d" eclim-java-find-declaration)
    ("r" eclim-java-find-references)
    ("g" eclim-java-generate-getter)
    ("s" eclim-java-generate-setter)
    ("c" eclim-java-constructor))
  :general
  (general-nmap :keymaps 'eclim-mode-map
                "M-t j" 'geek/hydra-eclim-menu/body)
  :config
  (setq eclim-executable "eclim"
        eclimd-executable "eclimd"
        eclim-use-yasnippet nil
        help-at-pt-display-when-idle t
        help-at-pt-time-delay 0.1
        eclimd-default-workspace (expand-file-name "~/hack/eclipse-workspace")
        eclimd-autostart nil)
  (help-at-pt-set-timer)
  (add-hook 'after-save-hook #'geek/eclim-after-save-hook))

(use-package clojure-mode
  :config (setq clojure-indent-style :always-indent))

(use-package groovy-mode
  :init (setq c-basic-offset 4))

(use-package flyspell
  :if (executable-find "hunspell")
  :init
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (add-hook 'org-mode-hook  #'flyspell-mode))

(use-package flyspell-correct-ivy
  :ensure flyspell
  :bind ("<f2>" . flyspell-correct-previous-word-generic))

(use-package org
  :config
  (setq org-hide-leading-stars t
        org-src-fontify-natively t)
  (set-face-attribute 'org-document-title nil :height 1.0))
;;; init.el ends here
