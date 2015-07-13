(custom-set-variables
 ;; appearances
 '(global-font-lock-mode t)
 '(line-number-mode t)
 '(column-number-mode t)
 '(transient-mark-mode t)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(blink-cursor-mode nil)

 ;; package
 '(package-archives
    '(("melpa" . "http://melpa.milkbox.net/packages/")))
 '(package-enable-at-startup nil)

 ;; mode
 '(json-reformat:indent-width 2)
 '(show-paren-mode t)

 ;; general
 '(confirm-kill-emacs (quote y-or-n-p))
 '(echo-keystrokes 0.1)
 '(inhibit-startup-message t)
 '(make-pointer-invisible t)
 '(vc-handled-backends '(git))

 ;; files
 '(delete-auto-save-files t)
 '(read-file-name-completion-ignore-case t)
 '(vc-follow-symlinks t)

 ;; formatting
 '(electric-indent-mode nil)
 '(fill-column 79)
 '(indent-tabs-mode nil))
