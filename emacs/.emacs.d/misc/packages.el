;; Download the packages used in init
(require 'package)
(setq package-user-dir (concat user-emacs-directory "elpa"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(package-refresh-contents)

(defvar my/install-packages
  '(auctex
    cider
    company
    expand-region
    flx-ido
    git-gutter
    helm
    helm-descbinds
    helm-swoop
    hideshow
    ido
    ido-vertical-mode
    js2-mode
    json-mode
    magit
    multiple-cursors
    popwin
    rainbow-delimiters
    smartparens
    smex
    smooth-scrolling
    switch-window
    undo-tree
    use-package))

(dolist (pack my/install-packages)
  (unless (package-installed-p pack)
    (package-install pack)))
