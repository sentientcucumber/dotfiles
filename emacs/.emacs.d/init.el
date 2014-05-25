;;  setup packages  ----------------------------------------------------- ;;
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;;  setup theme  -------------------------------------------------------- ;;
(load-theme 'solarized-dark t)

;;  basic settings  ----------------------------------------------------- ;;

;; stop making redonk backup files
(setq make-backup-files nil)

;; remove highlight line
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

;; rebind ctrl-t to ctrl-x for dvorak
(define-key key-translation-map "\C-t" "\C-x")

;; sane y or n bindings to yes or no questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; move one line, regardless of whether or not it is wrapped
(setq line-move-visual t)


;; tell emacs I'm on mac
(defun macosx-p ()
  (eq system-type 'darwin))

;; integrate with osx clipboard
(defun copy-from-osx ()
  (shell-command-to-string "/usr/bin/pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "/usr/bin/pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(when (macosx-p)
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

(line-number-mode 1)
(column-number-mode 1)

(setq read-file-name-completion-ignore-case t)

;;  c++ style  ---------------------------------------------------------- ;;
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tem\\'" . c++-mode))

(setq-default indent-tabs-mode nil)
(setq c-default-style "linux"
      c-basic-offset 4)

(setq ring-bell-function (lambda()))


;; don't show startup message
(setq inhibit-startup-message t)

;; highlight fixme and todo
;; (defun my/add-watchwords ()
;;   (font-lock-add-keywords
;;    nil '(("\\<\\(FIXME\\|TODO\\|XXX\\|NOCOMMIT\\|@@@\\)\\>"
;;           1 '((:foreground "pink") (:weight bold)) t))))

;; (add-hook 'prog-mode-hook 'my/add-watchwords)

(menu-bar-mode -1)
(when (window-system)
  (set-scroll-bar-mode 'nil)
  (mouse-wheel-mode t))
(tool-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode -1)

;; (global-font-lock-mode t)


;; ;; Uniquify buffers, using angle brackets, so you get foo and foo<2>:
;; (use-package uniquify
;;   :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; ;; enable regex where normal search and replace
;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; (global-set-key (kbd "M-%") 'query-replace-regexp)

;; ;; enable imenu, bind to c-x c-i
;; (set-default 'imenu-auto-rescan t)
;; (global-set-key (kbd "C-x C-i") 'imenu)


;; (setq-default ispell-program-name "aspell")
;; (setq ispell-extra-args '("--sug-mode=ultra" "--ignore=3"))
;; (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))

;; ;; enable flyspell
;; (autoload 'flyspell-mode "flyspell" "spell checking at runtime")
;; (eval-after-load "flyspell"
;;   '(progn
;;      (define-key flyspell-mode-map (kbd "M-n") 'flyspell-goto-next-error)
;;      (define-key flyspell-mode-map (kbd "M-.") 'ispell-word)))

;; (setq ispell-personal-dictionary "~/.flydict")

;; (ido-mode 1)
;; (setq ido-use-virtual-buffers nil)
;; (setq ido-enable-prefix nil
;;       ido-enable-flex-matching t
;;       ido-auto-merge-work-directories-length nil
;;       ido-create-new-buffer 'always
;;       ido-use-filename-at-point 'guess
;;       ido-max-prospects 10)

;; (use-package flx-ido
;;   :init (flx-ido-mode 1)
;;   :config
;;   (progn
;;     (setq ido-use-faces nil)))

;; (use-package idle-highlight-mode
;;   :init
;;   (progn
;;     (add-hook 'prog-mode-hook
;;               (lambda ()
;;                 (idle-highlight-mode t)))))

;; (use-package smart-mode-line
;;   :init (progn
;;           (setq sml/mode-width 'full)
;;           (sml/setup)))

;; (use-package smooth-scrolling
;;   :init (setq smooth-scroll-margin 4))

;; (use-package yasnippet
;;   :disabled t
;;   :defer t
;;   :commands yas-minor-mode-on
;;   :diminish yas-minor-mode
;;   :init
;;   (progn
;;     (dolist (hook '(clojure-mode-hook
;;                     org-mode-hook))
;;       (add-hook hook 'yas-minor-mode-on)))
;;   :config
;;   (progn
;;     ;; snippet-mode for *.yasnippet files
;;     (add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))
;;     (yas-reload-all)))

;; (use-package smartparens
;;   :init
;;   (progn
;;     (require 'smartparens-config)
;;     (smartparens-global-mode t)
;;     ;; (smartparens-global-strict-mode t)
;;     (show-smartparens-global-mode t))
;;   :config
;;   (progn
;;     (add-hook 'sh-mode-hook
;;               (lambda ()
;;                 ;; Remove when https://github.com/Fuco1/smartparens/issues/257
;;                 ;; is fixed
;;                 (setq sp-autoescape-string-quote nil)))

;;     (define-key sp-keymap (kbd "C-(") 'sp-forward-barf-sexp)
;;     (define-key sp-keymap (kbd "C-)") 'sp-forward-slurp-sexp)
;;     (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
;;     (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
;;     (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
;;     (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
;;     (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
;;     (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
;;     (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
;;     (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)
;;     (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
;;     (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
;;     (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
;;     (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)
;;     ;; (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
;;     ;; (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)
;;     (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
;;     (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)
;;     (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
;;     (define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
;;     (define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
;;     (define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)
;;     (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
;;     (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)
;;     (define-key sp-keymap (kbd "H-t") 'sp-prefix-tag-object)
;;     (define-key sp-keymap (kbd "H-p") 'sp-prefix-pair-object)
;;     (define-key sp-keymap (kbd "H-s c") 'sp-convolute-sexp)
;;     (define-key sp-keymap (kbd "H-s a") 'sp-absorb-sexp)
;;     (define-key sp-keymap (kbd "H-s e") 'sp-emit-sexp)
;;     (define-key sp-keymap (kbd "H-s p") 'sp-add-to-previous-sexp)
;;     (define-key sp-keymap (kbd "H-s n") 'sp-add-to-next-sexp)
;;     (define-key sp-keymap (kbd "H-s j") 'sp-join-sexp)
;;     (define-key sp-keymap (kbd "H-s s") 'sp-split-sexp)

;;     (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
;;     ;; Remove '' pairing in elisp because quoting is used a ton
;;     (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

;;     (sp-with-modes '(html-mode sgml-mode)
;;       (sp-local-pair "<" ">"))

;;     (sp-with-modes sp--lisp-modes
;;       (sp-local-pair "(" nil :bind "C-("))))

;; (use-package flycheck
;;   :disabled t
;;   :diminish "fc"
;;   :init
;;   (progn
;;     (add-hook 'after-init-hook #'global-flycheck-mode)
;;     ;; disable the annoying doc checker))
;;     (setq-default flycheck-disabled-checkers
;;                   '(emacs-lisp-checkdoc))))

;; (defun magit-browse ()
;;   (interactive)
;;   (let ((url (with-temp-buffer
;;                (unless (zerop (call-process-shell-command "git remote -v" nil t))
;;                  (error "Failed: 'git remote -v'"))
;;                (goto-char (point-min))
;;                (when (re-search-forward "github\\.com[:/]\\(.+?\\)\\.git" nil t)
;;                  (format "https://github.com/%s" (match-string 1))))))
;;     (unless url
;;       (error "Can't find repository URL"))
;;     (browse-url url)))

;; (use-package magit
;;   :bind ("M-g M-g" . magit-status)
;;   :config
;;   (progn
;;     (define-key magit-mode-map (kbd "C-c C-b") 'magit-browse)
;;     (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
;;     ;; faces
;;     ;; (set-face-attribute 'magit-branch nil
;;     ;;                     :foreground "yellow" :weight 'bold :underline t)
;;     (set-face-attribute 'magit-item-highlight nil
;;                         :background nil)
;;     (custom-set-variables '(magit-set-upstream-on-push (quote dontask)))))

;; ;; c-c p prefix for searching in projects
;; (use-package projectile
;;   :init (progn
;;           (projectile-global-mode)
;;           (defconst projectile-mode-line-lighter " P")))

;; (use-package git-gutter
;;   :defer t
;;   :diminish git-gutter-mode
;;   :init (progn
;;           (add-hook 'prog-mode-hook
;;                     (lambda ()
;;                       (git-gutter-mode t)
;;                       (global-set-key (kbd "C-x C-a") 'git-gutter:toggle)
;;                       (global-set-key (kbd "C-x =") 'git-gutter:popup-hunk)
;;                       (global-set-key (kbd "C-c P") 'git-gutter:previous-hunk)
;;                       (global-set-key (kbd "C-c N") 'git-gutter:next-hunk)
;;                       (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
;;                       (global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
;;                       (global-set-key (kbd "C-c G") 'git-gutter:popup-hunk)))))

;; (use-package helm
;;   :init
;;   (progn
;;     (require 'helm-config)
;;     (use-package helm-descbinds
;;       :init (helm-descbinds-mode t)))
;;   :config
;;   (progn
;;     (setq helm-idle-delay 0.1
;;           helm-input-idle-delay 0
;;           helm-candidate-number-limit 500)
;;     (define-key helm-map (kbd "C-p")   'helm-previous-line)
;;     (define-key helm-map (kbd "C-n")   'helm-next-line)
;;     (define-key helm-map (kbd "C-M-n") 'helm-next-source)
;;     (define-key helm-map (kbd "C-M-p") 'helm-previous-source)
;;     (defun helm-httpstatus ()
;;       (interactive)
;;       (helm-other-buffer '(helm-httpstatus-source) "*helm httpstatus*"))

;;     (defun helm-clj-http ()
;;       (interactive)
;;       (helm-other-buffer '(helm-clj-http-source) "*helm clj-http flags*"))

;;     (global-set-key (kbd "C-c M-C-h") 'helm-httpstatus)
;;     (global-set-key (kbd "C-c M-h") 'helm-clj-http)))

;; (use-package helm-ag
;;   :init (bind-key "C-M-s" 'helm-ag-this-file))

;; (use-package company
;;   :defer t
;;   :diminish company-mode
;;   :init (add-hook 'after-init-hook 'global-company-mode)
;;   :config
;;   ;; Tiny delay before completion
;;   (setq company-idle-delay 0.1
;;         ;; min prefix of 2 chars
;;         company-minimum-prefix-length 4))

;; (use-package smart-tab)

;; (use-package ido-ubiquitous)

;; (use-package popwin
;;   :config
;;   (progn
;;     (global-set-key (kbd "C-'") popwin:keymap)

;;     (defvar popwin:special-display-config-backup popwin:special-display-config)
;;     (setq display-buffer-function 'popwin:display-buffer)

;;     (push
;;      '("*Compile-Log*" :noselect t :height 10) popwin:special-display-config)
;;     (push
;;      '("*Messages*" :height 10) popwin:special-display-config)))

;; (use-package ido-vertical-mode
;;   :init (ido-vertical-mode t))

;; (use-package smex
;;   :init
;;   (progn
;;     (global-set-key [(meta x)]
;;                     (lambda ()
;;                       (interactive)
;;                       (or (boundp 'smex-cache)
;;                           (smex-initialize))
;;                       (global-set-key [(meta x)] 'smex)
;;                       (smex)))

;;     (global-set-key [(shift meta x)]
;;                     (lambda ()
;;                       (interactive)
;;                       (or (boundp 'smex-cache)
;;                           (smex-initialize))
;;                       (global-set-key [(shift meta x)] 'smex-major-mode-commands)
;;                       (smex-major-mode-commands))))
;;   :config
;;   (progn
;;     (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))

;; (global-set-key (kbd "C-x M-o") 'helm-occur)
;; (global-set-key (kbd "C-x C-o") 'helm-occur)
;; (global-set-key (kbd "M-y")     'helm-show-kill-ring)
;; (global-set-key (kbd "C-h a")   'helm-apropos)
;; (global-set-key (kbd "C-x C-i") 'helm-imenu)
;; (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
;; (global-set-key (kbd "C-x f") 'projectile-find-file)
;; (global-set-key (kbd "C-x C-l") 'toggle-truncate-lines)
;; (global-set-key (kbd "M-'") 'other-window)
;; (global-set-key (kbd "C-c n") 'cleanup-buffer)

;; (defun untabify-buffer ()
;;   (interactive)
;;   (untabify (point-min) (point-max)))

;; (defun indent-buffer ()
;;   (interactive)
;;   (indent-region (point-min) (point-max)))

;; (defun cleanup-buffer ()
;;   "Perform a bunch of operations on the whitespace content of a buffer."
;;   (interactive)
;;   (indent-buffer)
;;   (untabify-buffer)
;;   (delete-trailing-whitespace))

;; (defun my/insert-lod ()
;;   "Well. This is disappointing."
;;   (interactive)
;;   (insert "ಠ_ಠ"))

;; (global-set-key (kbd "C-c M-d") 'my/insert-lod)

;; (setq custom-file "~/.emacs.d/custom.el")
;; (when (file-exists-p custom-file)
;;   (load custom-file))

;; (set-face-attribute 'region nil :background "#363636")
;; (set-face-attribute 'default nil :background "#262626")
