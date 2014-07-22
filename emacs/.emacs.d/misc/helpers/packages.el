;; I stole this from dakrone, shhhhhhhhh
(require 'package)
(setq package-user-dir (concat user-emacs-directory "elpa"))
(add-to-list 'package-archives 
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(package-refresh-contents)

(defvar my/install-packages
  '(flyspell smooth-scrolling column-marker flycheck use-package
    flycheck-tip iedit popwin undo-tree auto-complete magit popup fuzzy))

(dolist (pack my/install-packages)
  (unless (package-installed-p pack)
    (package-install pack)))
