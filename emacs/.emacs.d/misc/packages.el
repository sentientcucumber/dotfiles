;; I stole this from dakrone, shhhhhhhhh
(require 'package)
(setq package-user-dir (concat user-emacs-directory "elpa"))
(add-to-list 'package-archives 
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(package-refresh-contents)

(defvar my/install-packages
  '(use-package
     uniquify
     smooth-scrolling
     undo-tree
     magit
     smartparens
     smex
     expand-region
     hideshow
     rainbow-delimiters
     ido
     flx-ido
     ido-vertical-mode
     company
     popwin
     git-gutter
     flycheck
     flycheck-tip
     js2-mode
     dired
     dired-x
     golden-ratio
     multiple-cursors)

  (dolist (pack my/install-packages)
    (unless (package-installed-p pack)
      (package-install pack)))
