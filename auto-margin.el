;; This automatically centers windows in large frames by adding hooks
;; to modify window margins whenever the window layout may have changed.

;; Controls max width for a centered window
(setq custom-frame-width 100)
(setq custom-min-margin 10)

(defun split-window-prefer-horizontal (&optional window)
  "Modified version of `split-window-sensibly' that splits horizontally
   by default when allowed."
  (interactive)
  (if (< (frame-width (window-frame window))
         split-width-threshold)
      ;; use the default behavior if the frame isn't wide enough to
      ;; support two full-size horizontal windows
      (split-window-sensibly window)
    (let ((window (or window (selected-window))))
      (destructuring-bind (mleft . mright) (window-margins window)
        ;; * Remove the existing window margins first, otherwise they will
        ;;   interfere with splitting calculations.
        ;; * If a split is performed, it will trigger an
        ;;   `autoset-window-margins' to set proper margin values.
        ;; * If no split is performed, the margins are set back to
        ;;   their existing values.
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
            ;; No split, restore existing window margins
            (and (set-window-margins window mleft mright)
                 nil))))))

;; Use this in place of `split-window-sensibly'
(setq split-window-preferred-function 'split-window-prefer-horizontal)
;;(setq split-window-preferred-function 'split-window-sensibly)

;; Set a low height threshold to generally allow vertical splits
;; when window is not wide enough for horizontal split.
;;(setq split-height-threshold 20)

;; or set high height threshold to avoid automatic vertical splitting of
;; both window columns?
;;(setq split-height-threshold 40)

;; Set a high width threshold to use a horizontal split whenever
;; the window is wide enough.
;; (setq split-width-threshold 160)

(defun autoset-window-margins (&optional window &rest args)
  (let ((w (or (and (windowp window) window)
               (selected-window))))
    (let* ((ws (window-size w t))
           (mtotal (min (- ws custom-frame-width 1)
                        (- (floor (/ ws 2)) 4))))
      (if (>= mtotal (* 2 custom-min-margin))
          (let ((ms (floor (/ (- ws custom-frame-width 1) 2))))
            (set-window-margins w ms ms))
        (set-window-margins w 0 0)))))
(defun remove-frame-margins (&optional frame)
  (let ((frame (or (and (framep frame) frame)
                   (selected-frame))))
    (dolist (window (window-list frame))
      (set-window-margins window 0 0))))
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
