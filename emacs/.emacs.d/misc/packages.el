;; Download the packages used in init
(require 'package)
(setq package-user-dir (concat user-emacs-directory "elpa"))
(add-to-list 'package-archives 
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(package-refresh-contents)

(defvar my/install-packages
  '(use-package
     smooth-scrolling
     undo-tree             ; visualize undo tree
     magit                 ; git interface
     smartparens           ; balance parens
     smex
     expand-region
     hideshow
     rainbow-delimiters    ; color parens and braces
     ido
     flx-ido
     ido-vertical-mode
     company               ; complete any
     popwin
     git-gutter
     flycheck
     flycheck-tip
     js2-mode              ; a saner way of editing javascript
     dired-x
     golden-ratio          ; balance windows 
     multiple-cursors
     web                   ; http client
     helm                  ; incremental completion
     helm-descbinds        ; search current keybindings
     helm-swoop)           ; search by pattern in files

  (dolist (pack my/install-packages)
    (unless (package-installed-p pack)
      (package-install pack)))
