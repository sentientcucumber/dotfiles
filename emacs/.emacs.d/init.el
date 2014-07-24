;; setup packages and melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(setq package-enable-at-startup nil)

;; key mappings
(define-key key-translation-map "\C-t" "\C-x")
(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-x C-l") 'toggle-truncate-lines)
(global-set-key (kbd "C-x +") 'balance-windows-area)
(defalias 'yes-or-no-p 'y-or-n-p)

;; default settings
(setq-default default-tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default find-file-visit-truename nil)
(add-hook 'after-init-hook #'global-subword-mode)

;; make it purty... or at least not annoying
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq ring-bell-function (lambda()))
(setq inhibit-startup-message t initial-major-mode 'fundamental-mode)
(blink-cursor-mode -1)
(setq make-pointer-invisible t)
(line-number-mode t)
(column-number-mode t)
(setq echo-keystrokes 0.1)
(setq line-move-visual t)

;; this nifty bit lets the emacs kill ring and mac's buffer join in harmony
(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen nil)
  (setq insert-directory-program "gls")
  (setq dired-listing-switches "-aBhl --group-directories-first")
  (defun copy-from-osx ()
    (shell-command-to-string "/usr/bin/pbpaste"))
  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "/usr/bin/pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  (setq interprogram-cut-function 'paste-to-osx
        interprogram-paste-function 'copy-from-osx))

;; setup c modes
(defun my/c-mode-init ()
  (c-set-style "k&r")
  (c-toggle-electric-state -1)
  (define-key c-mode-map (kbd "C-c o") 'ff-find-other-file)
  (define-key c++-mode-map (kbd "C-c o") 'ff-find-other-file)
  (hs-minor-mode 1)
  (setq c-basic-offset 4))

(add-hook 'c-mode-hook #'my/c-mode-init)
(add-hook 'c++-mode-hook #'my/c-mode-init)

;; miscellaneous settings
(transient-mark-mode t)
(setq read-file-name-completion-ignore-case t)
(when (window-system)
  (set-scroll-bar-mode 'nil)
  (mouse-wheel-mode t)
  (tooltip-mode -1)) 

(defun my/add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|NOCOMMIT\\)\\>"
          1 '((:foreground "#d7a3ad") (:weight bold)) t))))
(add-hook 'prog-mode-hook 'my/add-watchwords)

(defun my/insert-lod ()
  "Well. This is disappointing."
  (interactive)
  (insert "ಠ_ಠ"))

(global-set-key (kbd "C-c M-d") 'my/insert-lod)

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

;; package setup!
(require 'use-package)

(use-package column-marker
  :config
  (progn
    (column-marker-1 80)))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package flyspell
  :config
  (define-key flyspell-mode-map (kbd "M-n") 'flyspell-goto-next-error)
  (define-key flyspell-mode-map (kbd "M-.") 'ispell-word))
(setq-default ispell-program-name "aspell")
(setq ispell-personal-dictionary "~.emacs.d/flydict/.flydict"
      ispell-extra-args '("--sug-mode=ultra" "--ignore=3"))
(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))

(use-package smooth-scrolling
  :config
  (setq smooth-scroll-margin 4))

;; this isn't working.. may need to understand use-package more
;; (use-package flycheck
;;   :bind (("M-g M-n" . flycheck-next-error)
;;          ("M-g M-p" . flycheck-previous-error)
;;          ("M-g M-=" . flycheck-list-errors))
;;   :config
;;   (progn
;;     (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
;;     (use-package flycheck-tip
;;       :config
;;       (add-hook 'flycheck-mode-hook
;;                 (lambda ()
;;                   (global-set-key (kbd "C-c C-n") 'flycheck-tip-cycle)
;;                   (global-set-key (kbd "C-c C-p") 'flycheck-tip-cycle-reverse))))))

;; this isn't working, if I take it the bind off, it works great, otherwise it sucks
;; (use-package iedit
;;   :bind ("C-;" . iedit-mode))

(use-package popwin
  :bind ("C-'" . popwin:keymap)
  :config
  (progn
    (defvar popwin:special-display-config-backup popwin:special-display-config)
    (setq display-buffer-alist 'popwin:display-buffer)

    ;; basic
    (push '("*Help*" :stick t :noselect t) popwin:special-display-config)

    ;; magit
    (push '("*magit-process*" :stick t) popwin:special-display-config)

    ;; dictionaly
    (push '("*dict*" :stick t) popwin:special-display-config)
    (push '("*sdic*" :stick t) popwin:special-display-config)

    ;; man
    (push '(Man-mode :stick t :height 20) popwin:special-display-config)

    ;; git-gutter
    (push '("*git-gutter:diff*" :width 0.5 :stick t)
          popwin:special-display-config)))

(use-package undo-tree
  :init (global-undo-tree-mode)
  :diminish ""
  :config
  (progn
    (define-key undo-tree-map (kbd "C-x u") 'undo-tree-visualize)
    (define-key undo-tree-map (kbd "C-/") 'undo-tree-undo)))

(use-package auto-complete
  :disabled t
  :defer t
  :init (progn
          (use-package popup)
          (use-package fuzzy)
          (use-package auto-complete-config)
          ;; Enable auto-complete mode other than default enable modes
          (add-to-list 'ac-modes 'cider-repl-mode)
          (global-auto-complete-mode t)
          (ac-config-default))
  :config
  (progn
    (define-key ac-complete-mode-map (kbd "M-n") 'ac-next)
    (define-key ac-complete-mode-map (kbd "M-p") 'ac-previous)
    (define-key ac-complete-mode-map (kbd "C-s") 'ac-isearch)
    (define-key ac-completing-map (kbd "<tab>") 'ac-complete)))

(use-package magit
  :bind ("M-g M-g" . magit-status)
  :config
  (progn
    (defun magit-browse ()
      (interactive)
      (let ((url (with-temp-buffer
                   (unless (zerop (call-process-shell-command "git remote -v" nil t))
                     (error "Failed: 'git remote -v'"))
                   (goto-char (point-min))
                   (when (re-search-forward "github\\.com[:/]\\(.+?\\)\\.git" nil t)
                     (format "https://github.com/%s" (match-string 1))))))
        (unless url
          (error "Can't find repository URL"))
        (browse-url url)))

    (define-key magit-mode-map (kbd "C-c C-b") 'magit-browse)
    (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
    (custom-set-variables '(magit-set-upstream-on-push (quote dontask)))))

