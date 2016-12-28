(setq custom-frame-width 100)
(setq custom-frame-height 58)

(defun set-custom-theme ()
  (if (some #'display-graphic-p (frame-list))
      (setq custom-emacs-theme 'zenburn)
    (setq custom-emacs-theme 'sanityinc-tomorrow-night-rxvt)))

;;(set-frame-font "Sauce Code Pro Medium:pixelsize=32")
;;(set-frame-font "Inconsolata for Powerline:pixelsize=36")
;;(window-size (selected-window))
;;(window-margins (selected-window))
