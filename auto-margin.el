;; This automatically centers windows in large frames by adding hooks
;; to modify window margins whenever the window layout may have changed.

(require 'cl-lib)

;; Controls max width for a centered window
(defvar auto-margin/custom-frame-width 110)
(defvar auto-margin/custom-min-margin 20)

(defun split-window-prefer-horizontal (&optional window)
  "Modified version of `split-window-sensibly' that splits horizontally
   by default when allowed."
  (interactive)
  (let ((window (or window (selected-window))))
    (if (< (frame-width (window-frame window))
           split-width-threshold)
        ;; use the default behavior if the frame isn't wide enough to
        ;; support two full-size horizontal windows
        (split-window-sensibly window)
      (set-window-margins window 0 0)
      (or (and (window-splittable-p window t)
               ;; Split window horizontally.
               (with-selected-window window
                 (split-window-right)))
          (and (window-splittable-p window)
               ;; Split window vertically.
               (with-selected-window window
                 (split-window-below)))
          (and
           ;; If WINDOW is the only usable window on its frame (it is
           ;; the only one or, not being the only one, all the other
           ;; ones are dedicated) and is not the minibuffer window, try
           ;; to split it vertically disregarding the value of
           ;; `split-height-threshold'.
           (let ((frame (window-frame window)))
             (or
              (eq window (frame-root-window frame))
              (catch 'done
                (walk-window-tree (lambda (w)
                                    (unless (or (eq w window)
                                                (window-dedicated-p w))
                                      (throw 'done nil)))
                                  frame nil 'nomini)
                t)))
           (not (window-minibuffer-p window))
           (let ((split-height-threshold 0))
             (when (window-splittable-p window)
               (with-selected-window window
                 (split-window-below)))))))))

(defun split-window-auto ()
  (interactive)
  ;; Bind threshold to allow vertical split from interactive calls
  (let ((split-height-threshold 40))
    (let ((new-window (split-window-prefer-horizontal)))
      (unless (null new-window)
        (select-window new-window))
      new-window)))

;; Use this in place of `split-window-sensibly'
(setq split-window-preferred-function 'split-window-prefer-horizontal)

;; Set a low height threshold to generally allow vertical splits
;; when window is not wide enough for horizontal split.
;;(setq split-height-threshold 20)

;; or set high height threshold to avoid automatic vertical splitting of
;; both window columns.
;;(setq split-height-threshold 40)

;; Set a high width threshold to use a horizontal split whenever
;; the window is wide enough.
;; (setq split-width-threshold 160)

(defun autoset-window-margins (&optional window &rest args)
  (let ((w (or (and (windowp window) window)
               (selected-window))))
    (let* ((ws (window-size w t))
           (mtotal (min (- ws auto-margin/custom-frame-width 1)
                        (- (floor (/ ws 2)) 4))))
      (if (>= mtotal (* 2 auto-margin/custom-min-margin))
          (let ((ms (floor (/ (- ws auto-margin/custom-frame-width 1) 2))))
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
                window-size-change-functions
                after-make-frame-functions))
  (add-hook hook 'autoset-frame-margins))

;;(add-hook 'pre-redisplay-functions 'autoset-window-frame-margins)
;;(add-hook 'post-command-hook 'autoset-frame-margins)
