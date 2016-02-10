(setq custom-frame-width 100)
(setq custom-frame-height 58)

(setq custom-font "Sauce Code Pro Medium:pixelsize=25")

(setq custom-emacs-theme 'moe-dark)
;;(setq custom-emacs-theme 'sanityinc-tomorrow-night)
;;(setq custom-emacs-theme 'gruvbox)
'(if (null window-system)
     (setq custom-emacs-theme 'sanityinc-tomorrow-night-rxvt)
   (setq custom-emacs-theme 'sanityinc-tomorrow-night))

