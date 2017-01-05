(defvar arc/skeleton-location-stack nil
  "Markers for locations saved in skeleton-positions")

(add-hook 'skeleton-end-hook 'skeleton-make-markers)

(defun arc/skeleton-make-markers ()
  (while arc/skeleton-location-stack
    (set-marker (pop arc/skeleton-location-stack) nil))
  (setq arc/skeleton-location-stack
        (mapcar 'copy-marker (reverse skeleton-positions))))

(defun arc/skeleton-next-position (&optional reverse)
  "Jump to next position in skeleton. REVERSE - Jump to previous
position in skeleton"
  (interactive "P")
  (let* ((positions (mapcar 'marker-position arc/skeleton-location-stack))
         (positions (if reverse (reverse positions) positions))
         (comp (if reverse '> '<))
         pos)
    (when positions
      (if (catch 'break
            (while (setq pos (pop positions))
              (when (funcall comp (point) pos)
                (throw 'break t))))
          (goto-char pos)
        (goto-char (marker-position
                    (car arc/skeleton-location-stack)))))))
