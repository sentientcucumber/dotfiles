;; Package initialization 
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; evil should be one of the first things loaded. If it's not loaded, I can't
;; navigate through emacs very well.
(use-package evil
  :ensure t
  :demand
  :diminish undo-tree-mode
  :config
  ;; Practically a requirement if you're using evil-mode.
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ","))
 (evil-mode t))

(use-package powerline-evil
  :config (powerline-default-theme))

(set-frame-font "Fantasque Sans Mono 12")

(put 'erase-buffer 'disabled nil)
