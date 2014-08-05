;; I stole this from dakrone, shhhhhhhhh
(require 'package)
(setq package-user-dir (concat user-emacs-directory "elpa"))
(add-to-list 'package-archives 
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(package-refresh-contents)

(defvar my/install-packages
  '(column-marker company expand-region flx-ido flycheck flycheck-tip
    git-gutter hideshow ido ido-vertical-mode magit popwin smex
    smooth-scrolling undo-tree uniquify use-package dired-x dired+)

  (dolist (pack my/install-packages)
    (unless (package-installed-p pack)
      (package-install pack)))
