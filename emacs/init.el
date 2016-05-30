(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'use-package)

(use-package diminsh)

(use-package evil
  :ensure t
  :diminish undo-tree-mode
  :config
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ","))
 (evil-mode t))

(use-package powerline-evil
  :config (powerline-default-theme))
