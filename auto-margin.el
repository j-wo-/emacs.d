;; This automatically centers windows in large frames by adding hooks
;; to modify window margins whenever the window layout may have changed.

(defun split-window-prefer-horizontal (&optional window)
  "Modified version of split-window-sensibly that splits horizontally
   by default when allowed."
  (interactive)
  (let ((window (or window (selected-window))))
    (destructuring-bind (mleft . mright) (window-margins window)
      (set-window-margins window 0 0)
      (or (and (window-splittable-p window t)
               ;; Split window horizontally.
               (with-selected-window window
                 (split-window-right)))
          (and (window-splittable-p window)
               ;; Split window vertically.
               (with-selected-window window
                 (split-window-below)))
          (and (eq window (frame-root-window (window-frame window)))
               (not (window-minibuffer-p window))
               ;; If WINDOW is the only window on its frame and is not the
               ;; minibuffer window, try to split it vertically disregarding
               ;; the value of `split-height-threshold'.
               (let ((split-height-threshold 0))
                 (when (window-splittable-p window)
                   (with-selected-window window
                     (split-window-below)))))
          (and (set-window-margins window mleft mright)
               nil)))))
(setq split-window-preferred-function 'split-window-prefer-horizontal)
(setq split-height-threshold 44)
(setq split-width-threshold 160)

(defun autoset-window-margins (&optional window &rest args)
  (let ((w (or (and (windowp window) window)
               (selected-window))))
    (let* ((ws (window-size w t))
           (mtotal (min (- ws custom-frame-width 1)
                        (- (floor (/ ws 2)) 4)))
           (ms (floor (/ mtotal 2))))
      (if (>= ms 4)
          (set-window-margins w ms ms)
        (set-window-margins w 0 0)))))
(defun autoset-frame-margins (&optional frame &rest args)
  (let ((frame (or (and (framep frame) frame)
                   (selected-frame))))
    (mapc #'autoset-window-margins (window-list frame))))
(defun autoset-window-frame-margins (window)
  (autoset-frame-margins (window-frame window)))

(dolist (hook '(window-setup-hook
                ;; window-configuration-change-hook
                window-size-change-functions
                after-make-frame-functions
                ;; post-command-hook
                ;; buffer-list-update-hook
                ;; after-setting-font-hook
                ;; focus-in-hook
                ;; focus-out-hook
                ))
  (add-hook hook 'autoset-frame-margins))

(add-hook 'pre-redisplay-functions 'autoset-window-frame-margins)
