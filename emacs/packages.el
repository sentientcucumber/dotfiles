(require 'use-package)

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package smooth-scrolling
  :ensure t
  :config (setq smooth-scroll-margin 8))

(use-package undo-tree
  :ensure t
  :bind ("C-x u" . undo-tree-visualize))

(use-package magit
  :ensure t
  :bind ("M-g M-g" . magit-status)
  :config (setq magit-auto-rever-mode nil
                magit-last-seen-setup-instructions "1.4.0"))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :bind (("C-c (" . sp-forward-barf-sexp)
         ("C-c )" . sp-forward-slurp-sexp))
  :config (use-package smartparens-config)
  :init (add-hook 'prog-mode-hook 'smartparens-mode t))

(use-package expand-region
  :ensure t
  :bind (("C-c e"   . er/expand-region)
         ("C-c C-e" . er/contract-region))
  :config (pending-delete-mode t))

(use-package hideshow
  :ensure t
  :bind (("C-c TAB" . hs-toggle-hiding)
         ("C-\\"    . hs-toggle-hiding)
         ("M-\\"    . hs-hide-all)
         ("M-|"     . hs-show-all))
  :init (add-hook 'prog-mode-hook 'hs-minor-mode t))

(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package company
  :ensure t
  :diminish company-mode
  :init (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 3
        company-selection-wrap-around t
        company-dabbrev-downcase nil
        company-transformers '(company-sort-by-occurrence)))

(use-package popwin
  :ensure t
  :init
  :config
  (progn
    (popwin-mode t)
    (defvar popwin:special-display-config-backup popwin:special-display-config)
    (setq display-buffer-function 'popwin:display-buffer)
    (push '("*Completions*" :stick f :height 20 :position bottom :noselect t)
          popwin:special-display-config)
    (push '("*Warnings*" :stick t :height 20 :position bottom :noselect t)
          popwin:special-display-config)
    (push '(" *undo-tree*" :stick t :height 20 :position bottom :noselect t)
          popwin:special-display-config)
    (push '("*Comile-Log*" :stick f :noselect t)
          popwin:special-display-config)))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config (setq-default js2-basic-offset 2))

(use-package dired
  :bind ("C-x C-j" . dired-jump)
  :config (use-package dired-x
            :config (setq ls-lisp-dirs-first t
                          delete-by-moving-to-trash t
                          dired-dwim-target t)
            (define-key dired-mode-map (kbd "C-c C-u") 'dired-up-directory)
            (define-key dired-mode-map (kbd "C-x C-q") 'wdired-change-to-wdired-mode)
            (add-hook 'dired-mode-hook (lambda ()
                                         (hl-line-mode t)
                                         (toggle-truncate-lines t)))))

(use-package helm
  :ensure t
  :bind (("C-x b"   . helm-mini)
         ("C-x k"   . helm-mini)
         ("M-x"     . helm-M-x)
         ("C-x C-i" . helm-semantic-or-imenu)
         ("M-y"     . helm-show-kill-ring)
         ("C-x f"   . helm-find-files)
         ("C-h b"   . helm-descbinds))
  :config (progn
            (use-package helm-files)
            (use-package helm-config)
            (setq helm-buffers-fuzzy-matching t
                  helm-truncate-lines t)
            (use-package helm-swoop
              :bind
              (("M-i" . helm-swoop)
               ("M-I" . helm-multi-swoop)))
            (use-package helm-descbinds
              :init
              (helm-descbinds-mode t))))

(use-package eww
  :bind (("C-c w" . eww)
         ("C-c o" . eww-browse-with-external-browser)))

(use-package ido
  :ensure t
  :init (ido-mode t)
  :config
  (progn
    (setq ido-use-virtual-buffers nil
          ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-auto-merge-work-directories-length nil
          ido-create-new-buffer 'always
          ido-save-directory-list-file 
          (concat user-emacs-directory "misc/ido/.ido")))
  (use-package flx-ido
    :init (flx-ido-mode t)
    :config (setq ido-use-faces nil))
  (use-package ido-vertical-mode
    :init (ido-vertical-mode t)))

(use-package org
  :ensure t
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-iswitchb)
         ("C-c l" . org-store-link))
  :init (add-hook 'org-mode-hook (lambda ()
                               (set-fill-column 79)
                               (turn-on-auto-fill))))
