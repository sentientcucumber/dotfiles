;; setup melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(setq package-enable-at-startup nil)

;; miscellaneous settings
(define-key key-translation-map "\C-t" "\C-x")
(setq echo-keystrokes 0.1)
(transient-mark-mode t)
(setq ring-bell-function (lambda()))
(setq inhibit-startup-message t initial-major-mode 'fundamental-mode)
(line-number-mode t)
(column-number-mode t)
(setq read-file-name-completion-ignore-case t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq line-move-visual t)
(setq make-pointer-invisible t)
(setq-default fill-column 80)
(setq-default default-tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default find-file-visit-truename nil)
(setq require-final-newline t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(when (window-system)
  (set-scroll-bar-mode 'nil)
  (mouse-wheel-mode t)
  (tooltip-mode -1))
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-x C-l") 'toggle-truncate-lines)
(define-key global-map (kbd "RET") 'newline-and-indent)
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
(setq-default subword-mode t)
(setq-default find-file-visit-truename nil)
(setq require-final-newline t)
(global-set-key (kbd "C-x +") 'balance-windows-area)

(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

(defun my/add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|NOCOMMIT\\)\\>"
          1 '((:foreground "#d7a3ad") (:weight bold)) t))))

(add-hook 'prog-mode-hook 'my/add-watchwords)

(require 'use-package)

(defun setup-java ()
  (interactive)
  (define-key java-mode-map (kbd "M-,") 'pop-tag-mark)
  (defconst eclipse-java-style
    '((c-basic-offset . 4)
      (c-comment-only-line-offset . (0 . 0))
      ;; the following preserves Javadoc starter lines
      (c-offsets-alist . ((inline-open . 0)
                          (topmost-intro-cont    . +)
                          (statement-block-intro . +)
                          (knr-argdecl-intro     . 5)
                          (substatement-open     . +)
                          (substatement-label    . +)
                          (label                 . +)
                          (statement-case-open   . +)
                          (statement-cont        . ++)
                          (arglist-intro  . ++)
                          (arglist-close  . ++)
                          (arglist-cont-nonempty . ++)
                          (access-label   . 0)
                          (inher-cont     . ++)
                          (func-decl-cont . ++))))
    "Eclipse Java Programming Style")

  ;; Generic java stuff things
  (setq whitespace-line-column 140)
  (use-package column-marker
    :config
    (progn
      (column-marker-1 140)
      (column-marker-2 80)))
  (c-add-style "ECLIPSE" eclipse-java-style)
  (customize-set-variable 'c-default-style
                          (quote ((java-mode . "eclipse")
                                  (awk-mode . "awk")
                                  (other . "gnu"))))
  (c-set-offset 'arglist-cont-nonempty '++))

(add-hook 'java-mode-hook 'setup-java)

(defun my/c-mode-init ()
  (c-set-style "k&r")
  (c-toggle-electric-state -1)
  (define-key c-mode-map (kbd "C-c o") 'ff-find-other-file)
  (define-key c++-mode-map (kbd "C-c o") 'ff-find-other-file)
  (hs-minor-mode 1)
  (setq c-basic-offset 4))

(add-hook 'c-mode-hook #'my/c-mode-init)
(add-hook 'c++-mode-hook #'my/c-mode-init)

;; package setup
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(setq-default ispell-program-name "aspell")
(setq ispell-personal-dictionary "~.emacs/flydict/.flydict"
      ispell-extra-args '("--sug-mode=ultra" "--ignore=3"))
(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))

;; flyspell
(use-package flyspell
  :config
  (define-key flyspell-mode-map (kbd "M-n") 'flyspell-goto-next-error)
  (define-key flyspell-mode-map (kbd "M-.") 'ispell-word))

(use-package smooth-scrolling
  :config
  (setq smooth-scroll-margin 4))

;; (use-package flycheck
;;   :bind (("M-g M-n" . flycheck-next-error)
;;          ("M-g M-p" . flycheck-previous-error)
;;          ("M-g M-=" . flycheck-list-errors))
;;   :idle (global-flycheck-mode)
;;   :config
;;   (progn
;;     (setq-default flycheck-disabled-checkers
;;                   '(emacs-lisp-checkdoc))
;;     (use-package flycheck-tip
;;       :config
;;       (add-hook 'flycheck-mode-hook
;;                 (lambda ()
;;                   (global-set-key (kbd "C-c C-n") 'flycheck-tip-cycle)
;;                   (global-set-key (kbd "C-c C-p") 'flycheck-tip-cycle-reverse))))))

(use-package iedit
  :bind ("C-;" . iedit-mode))

(use-package popwin
  :bind ("C-'" . popwin:keymap)
  :config
  (progn
    (defvar popwin:special-display-config-backup popwin:special-display-config)
    (setq display-buffer-function 'popwin:display-buffer)

    ;; basic
    (push '("*Help*" :stick t :noselect t) popwin:special-display-config)
    (push '("*helm world time*" :stick t :noselect t) popwin:special-display-config)

    ;; magit
    (push '("*magit-process*" :stick t) popwin:special-display-config)

    ;; quickrun
    (push '("*quickrun*" :stick t) popwin:special-display-config)

    ;; dictionaly
    (push '("*dict*" :stick t) popwin:special-display-config)
    (push '("*sdic*" :stick t) popwin:special-display-config)

    ;; popwin for slime
    (push '(slime-repl-mode :stick t) popwin:special-display-config)

    ;; man
    (push '(Man-mode :stick t :height 20) popwin:special-display-config)

    ;; Elisp
    (push '("*ielm*" :stick t) popwin:special-display-config)
    (push '("*eshell pop*" :stick t) popwin:special-display-config)

    ;; pry
    (push '(inf-ruby-mode :stick t :height 20) popwin:special-display-config)

    ;; python
    (push '("*Python*"   :stick t) popwin:special-display-config)
    (push '("*Python Help*" :stick t :height 20) popwin:special-display-config)
    (push '("*jedi:doc*" :stick t :noselect t) popwin:special-display-config)

    ;; Haskell
    (push '("*haskell*" :stick t) popwin:special-display-config)
    (push '("*GHC Info*") popwin:special-display-config)

    ;; sgit
    (push '("*sgit*" :position right :width 0.5 :stick t)
          popwin:special-display-config)

    ;; git-gutter
    (push '("*git-gutter:diff*" :width 0.5 :stick t)
          popwin:special-display-config)

    ;; direx
    (push '(direx:direx-mode :position left :width 40 :dedicated t)
          popwin:special-display-config)

    (push '("*Occur*" :stick t) popwin:special-display-config)

    ;; prodigy
    (push '("*prodigy*" :stick t) popwin:special-display-config)

    ;; malabar-mode
    (push '("*Malabar Compilation*" :stick t :height 30)
          popwin:special-display-config)

    ;; org-mode
    (push '("*Org tags*" :stick t :height 30)
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

(use-package smartparens
  :config
  (progn
    (use-package smartparens-config)
    (add-hook 'sh-mode-hook
              (lambda ()
                ;; Remove when https://github.com/Fuco1/smartparens/issues/257
                ;; is fixed
                (setq sp-autoescape-string-quote nil)))

    ;; Remove the M-<backspace> binding that smartparens adds
    (let ((disabled '("M-<backspace>")))
      (setq sp-smartparens-bindings
            (cl-remove-if (lambda (key-command)
                            (member (car key-command) disabled))
                          sp-smartparens-bindings)))

    (define-key sp-keymap (kbd "C-(") 'sp-forward-barf-sexp)
    (define-key sp-keymap (kbd "C-)") 'sp-forward-slurp-sexp)
    (define-key sp-keymap (kbd "M-(") 'sp-forward-barf-sexp)
    (define-key sp-keymap (kbd "M-)") 'sp-forward-slurp-sexp)
    (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
    (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
    (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
    (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
    (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
    (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
    (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
    (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)
    (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
    (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
    (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
    (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)
    ;; (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
    ;; (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)
    (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
    (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)
    (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
    (define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
    (define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
    (define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)
    (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
    (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)
    (define-key sp-keymap (kbd "H-t") 'sp-prefix-tag-object)
    (define-key sp-keymap (kbd "H-p") 'sp-prefix-pair-object)
    (define-key sp-keymap (kbd "H-s c") 'sp-convolute-sexp)
    (define-key sp-keymap (kbd "H-s a") 'sp-absorb-sexp)
    (define-key sp-keymap (kbd "H-s e") 'sp-emit-sexp)
    (define-key sp-keymap (kbd "H-s p") 'sp-add-to-previous-sexp)
    (define-key sp-keymap (kbd "H-s n") 'sp-add-to-next-sexp)
    (define-key sp-keymap (kbd "H-s j") 'sp-join-sexp)
    (define-key sp-keymap (kbd "H-s s") 'sp-split-sexp)

    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    ;; Remove '' pairing in elisp because quoting is used a ton
    (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

    (sp-with-modes '(html-mode sgml-mode)
      (sp-local-pair "<" ">"))

    (sp-with-modes sp--lisp-modes
      (sp-local-pair "(" nil :bind "C-("))))

(add-hook 'prog-mode-hook
          (lambda ()
            (smartparens-global-mode t)
            (show-smartparens-global-mode t)))

(when (not (window-system))
    (use-package server
          :init
              (unless (server-running-p)
                      (server-start))))

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
