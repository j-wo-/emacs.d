(setq custom-frame-width 100)
(setq custom-frame-height 58)

(setq custom-emacs-theme 'moe-dark)
;;
;; 'moe-dark
;; 'sanityinc-tomorrow-night
;; 'sanityinc-solarized-light
;; 'gruvbox
;; 'base16-default-dark
;; 'material
;;
;; (switch-to-theme custom-emacs-theme)
;;
(when (and (eql custom-emacs-theme 'sanityinc-tomorrow-night)
	   (null window-system))
  (setq custom-emacs-theme 'sanityinc-tomorrow-night-rxvt))

;;(set-frame-font "Sauce Code Pro Medium:pixelsize=30")
;;(set-frame-font "Inconsolata for Powerline:pixelsize=33")
