(defun my/add-watchwords ()
  "Highlight FIXME and TODO in code"
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\)\\>"
          1 '((:foreground "#DC8CC3")) t))))
(add-hook 'prog-mode-hook 'my/add-watchwords)

(defun my/c-mode-init()
  "Various style settings for C."
  (c-set-style "k&r")
  (c-toggle-electric-state -1)
  (setq c-basic-offset 4))
(add-hook 'c-mode-hook #'my/c-mode-init)

(defun my/go-mode-init()
  "Various style settings for Go."
  (setq c-basic-offset 8
        indent-tabs-mode t))
(add-hook 'go-mode-hook #'my/go-mode-init)

(defun my/nxml-mode-init()
  "Various style settings for nXML."
  (setq subword-mode t
        nxml-child-indent 4
        nxml-slash-auto-complete-flag t))
(add-hook 'nxml-mode-hook #'my/nxml-mode-init)

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))
(global-set-key [f11] 'toggle-fullscreen)

(defun untabify-buffer()
  "Replace all tabs with spaces."
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer()
  "Indent the buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Indent, untabify, and delete trailing whitespace."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(add-hook 'prog-mode-hook
          (lambda ()
            (visual-line-mode t)
            (subword-mode t)))
