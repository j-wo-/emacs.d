;;; -*- lexical-binding: t -*-

(require 'init-base)
(require 'cl-lib)
(require 'faces)
(require 'use-package)
(use-package dash)

(use-package powerline
  ;;:pin melpa
  :if (not jeff/use-spaceline)
  :config
  (setq powerline-height 28
        ;; powerline-height 31
        ;; powerline-height 29
        ;; powerline-height 48
        powerline-default-separator 'arrow
        ;; powerline-default-separator 'utf-8
        powerline-display-buffer-size nil
        powerline-display-mule-info nil
        powerline-display-hud nil
        powerline-gui-use-vcs-glyph t
        powerline-text-scale-factor 0.95)
  (powerline-reset)
  (powerline-default-theme)
  (force-mode-line-update))

(when jeff/use-spaceline
  (load-local 'init-spaceline t))

(defun -th (name)
  (switch-to-theme name))

(when nil
  ;;;; Favorite themes
  (-th 'sanityinc-tomorrow-night)
  (-th 'sanityinc-tomorrow-eighties)
  (-th 'gruvbox-dark-soft)
  (-th 'gruvbox-dark-medium)
  (-th 'gruvbox-dark-hard)
  (-th 'base16-gruvbox-dark-soft)
  (-th 'base16-gruvbox-dark-medium)
  (-th 'base16-gruvbox-dark-hard)
  (-th 'zenburn)
  (-th 'material)
  (-th 'flatland)
  (-th 'alect-dark)
  (-th 'base16-eighties)
  (-th 'sanityinc-solarized-dark)
  (-th 'sanityinc-solarized-light)
  ;;;; More themes
  (-th 'base16-material)
  (-th 'base16-material-darker)
  (-th 'moe-dark)
  (-th 'alect-black)
  (-th 'base16-default-dark)
  (-th 'base16-tomorrow-night)
  (-th 'base16-tomorrow-night-eighties)
  (-th 'leuven-dark)
  (-th 'apropospriate-dark)
  (-th 'monokai)
  (-th 'molokai)
  (-th 'leuven)
  ;;(-th 'anti-zenburn)
  ;;(-th 'sanityinc-tomorrow-night-rxvt)
  ;;(-th 'spacemacs-dark)
  )

(defvar custom-emacs-theme
  (if (graphical?) 'sanityinc-tomorrow-night 'sanityinc-tomorrow-night-rxvt)
  ;; 'alect-dark
  ;; (if (graphical?) 'gruvbox-dark-soft 'sanityinc-tomorrow-night)
  ;; (if (graphical?) 'base16-eighties 'sanityinc-tomorrow-night)
  ;; (if (graphical?) 'zenburn 'sanityinc-tomorrow-night)
  ;; (if (graphical?) 'material 'sanityinc-tomorrow-night)
  )

(defvar override-faces nil)

(defun set-override-face (face spec)
  (face-spec-set face spec)
  (add-to-list 'override-faces face))

(defun set-override-faces (&rest face-specs)
  (dolist (fs face-specs)
    (cl-destructuring-bind (face spec) fs
      (set-override-face face spec))))

(defun reset-override-faces ()
  (dolist (face override-faces)
    (face-spec-set face nil 'reset))
  (setq override-faces nil))

(defcustom modeline-font
  ;;nil
  ;; "InputC3Mono Nerd Font:pixelsize=22"
  ;; "InputC3Mono Nerd Font 17"
  ;; "InputMono Nerd Font 16"
  ;; "InputMono Nerd Font:pixelsize=21"
  ;; "InputC4Mono Nerd Font:pixelsize=24"
  ;; "InputC4Mono Nerd Font 18"
  "InputC4Mono Nerd Font:pixelsize=23"
  "Alternate font used for modeline."
  :group 'jeff)

(defun switch-to-theme (theme)
  (cl-flet ((theme-p (s) (symbol-matches theme s)))
    (let (;;(lnum-font nil)
          ;;(lnum-font "Inconsolata 16")
          ;;(lnum-font "Inconsolata:pixelsize=22")
          (lnum-font "InputC4Mono Nerd Font 16")
          ;;(lnum-font "InputC3Mono Nerd Font:pixelsize=22")
          ;;(lnum-font "Inconsolata for Powerline:pixelsize=22")
          (lnum-weight1 'regular)
          (lnum-weight2 'regular))
      ;; load elpa package for theme
      (cl-eval-when (eval load)
        (cond ((theme-p "sanityinc-tomorrow")
               (use-package color-theme-sanityinc-tomorrow))
              ((theme-p "sanityinc-solarized")
               (use-package color-theme-sanityinc-solarized))
              ((theme-p "gruvbox")          (progn
                                              (use-package autothemer)
                                              (use-package gruvbox-theme
                                                :ensure nil
                                                :load-path "~/.emacs.d/gruvbox-theme")))
              ;; ((theme-p "spacemacs-dark")   (use-package spacemacs-theme))
              ((theme-p "material")         (use-package material-theme))
              ((theme-p "ample")            (use-package ample-theme))
              ((theme-p "base16")           (use-package base16-theme))
              ((theme-p "zenburn")          (use-package zenburn-theme))
              ((theme-p "anti-zenburn")     (use-package anti-zenburn-theme))
              ((theme-p "flatland")         (use-package flatland-theme))
              ((theme-p "moe")              (use-package moe-theme))
              ((theme-p "apropospriate")    (use-package apropospriate-theme))
              ((theme-p "molokai")          (use-package molokai-theme))
              ((theme-p "monokai")          (use-package monokai-theme))
              ((theme-p "leuven")           (use-package leuven-theme))
              ((theme-p "cyberpunk")        (use-package cyberpunk-theme))
              ((theme-p "alect")            (use-package alect-themes
                                              :ensure nil
                                              :load-path "~/.emacs.d/alect-themes"))))
      ;; disable any current themes
      (dolist (active-theme custom-enabled-themes)
        (disable-theme active-theme))
      ;; reset any modified face specs
      (reset-override-faces)
      (when lnum-font
        (set-override-faces `(line-number
                              ((t (:font ,lnum-font :weight ,lnum-weight1))))
                            `(line-number-current-line
                              ((t (:font ,lnum-font :weight ,lnum-weight2))))))
      (cond ((theme-p "alect-dark")
             (alect-create-theme dark))
            ((theme-p "alect-dark-alt")
             (alect-create-theme dark t))
            ((theme-p "alect-black")
             (alect-create-theme black))
            ((theme-p "alect-light")
             (alect-create-theme light)))
      ;; activate theme
      (cond ((eql theme 'moe-dark)   (moe-dark))
            ((eql theme 'moe-light)  (moe-light))
            (t                       (load-theme theme t)))
      (when modeline-font
        (set-override-faces
         `(mode-line (( t (:font ,modeline-font))))
         `(mode-line-inactive ((t (:font ,modeline-font))))))
      ;; set face specs based on theme
      ''(cond ((theme-p "zenburn")
               (set-override-faces
                `(vertical-border
                  ((t (:foreground "#7b7b6b"))))
                `(mode-line
                  ((t (:font ,modeline-font
                             :box nil
                             ;; :foreground "#8fb28f"
                             ;; :background "#2b2b2b"
                             ))))
                `(mode-line-inactive
                  ((t (:font ,modeline-font
                             ;; :foreground "#5f7f5f"
                             ;; :background "#383838"
                             :box nil))))))
              ((and nil (theme-p "gruvbox"))
               (set-override-faces
                ;; `(fringe ((t (:background "#373230"))))
                ;; `(fringe ((t (:background "#332c2a"))))
                `(fringe ((t (:background "#37312b"))))
                `(line-number
                  ((t (;; :foreground "#7c6f64" :background "#3c3836"
                       ;; :foreground "#6c5f54" :background "#363230"
                       ;; :foreground "#5f5046" :background "#37312b"
                       :foreground "#635448" :background "#37312b"
                       :font ,lnum-font :weight ,lnum-weight1))))
                `(line-number-current-line
                  ((t (:foreground "#fe8019" :background "#37312b"
                                   :font ,lnum-font :weight ,lnum-weight2))))
                `(mode-line
                  ((t (:font ,modeline-font :foreground "#d5c4a1" :background "#665c54"))))
                `(mode-line-inactive
                  ((t (:font ,modeline-font :foreground "#a89984" :background "#3c3836"))))))
              ((theme-p "alect")
               (set-override-faces))
              ((theme-p "eighties")
               (set-override-faces
                `(fringe ((t (:background "#404040"))))
                `(vertical-border ((t (:foreground "#505050"))))
                `(mode-line
                  ((t (:font ,modeline-font
                             :foreground "#e8e8e8"
                             :box (:line-width -2 :color "#4a4a4a" :style nil)
                             ;; :box (:line-width 0 :color "#555555" :style nil)
                             ))))
                `(mode-line-inactive
                  ((t (:font ,modeline-font
                             :foreground "#c0c0c0"
                             :box (:line-width -2 :color "#4a4a4a" :style nil)
                             ))))))
              ((and nil t)
               (set-override-faces
                `(fringe ((t (:background "#2a292c"))))
                ;; `(fringe ((t (:background "#373230"))))
                `(vertical-border ((t (:foreground "#505050"))))
                `(mode-line
                  ((t (:font ,modeline-font
                             :box (:line-width -1 :color "#555555" :style nil)
                             ;; :box nil
                             ))))
                `(mode-line-inactive
                  ((t (:font ,modeline-font
                             :box (:line-width -1 :color "#555555" :style nil)
                             ;; :box nil
                             )))))))
      ;; ensure powerline colors are updated
      (powerline-reset)
      (powerline-default-theme)
      (redraw-frame))))

(defun switch-custom-theme (&optional frame)
  (let ((frame (or (and (framep frame) frame)
                   (selected-frame))))
    (with-selected-frame frame
      (switch-to-theme custom-emacs-theme))))

;; modified from faces.el to rewrite #xxxxxx00 to #xxxxxx
(defun color-values (color &optional frame)
  "Return a description of the color named COLOR on frame FRAME.
COLOR should be a string naming a color (e.g. \"white\"), or a
string specifying a color's RGB components (e.g. \"#ff12ec\").

Return a list of three integers, (RED GREEN BLUE), each between 0
and either 65280 or 65535 (the maximum depends on the system).
Use `color-name-to-rgb' if you want RGB floating-point values
normalized to 1.0.

If FRAME is omitted or nil, use the selected frame.
If FRAME cannot display COLOR, the value is nil.

COLOR can also be the symbol `unspecified' or one of the strings
\"unspecified-fg\" or \"unspecified-bg\", in which case the
return value is nil."
  (when (and (stringp color)
             (string-equal (substring color 0 1) "#")
             (= (length color) (+ 1 8)))
    (setq color (substring color 0 7)))
  (cond
   ((member color '(unspecified "unspecified-fg" "unspecified-bg"))
    nil)
   ((memq (framep (or frame (selected-frame))) '(x w32 ns))
    (xw-color-values color frame))
   (t
    (tty-color-values color frame))))

(defun theme? (theme)
  (let ((theme (if (symbolp theme) (symbol-name theme) theme)))
    (symbol-matches custom-emacs-theme theme)))

(defun --set-paren-face-colors ()
  (use-package paren-face)
  (let ((paren-color  (cond ((theme? 'zenburn) "#808080")
                            ((theme? 'alect-dark) "#848484")
                            ((theme? 'gruvbox-dark-medium) "#787878")
                            ((theme? 'eighties) "#868686")
                            (t "#707070")))
        (square-color (cond ((theme? 'zenburn) "#bfc438")
                            ((theme? 'alect-dark) "#c4c830")
                            ((theme? 'gruvbox-dark-medium) "#cbcf3a")
                            ((theme? 'eighties) "#d4d648")
                            (t "#bbbf40")))
        (curly-color  (cond ((theme? 'zenburn) "#66a818")
                            ((theme? 'alect-dark) "#8bc018")
                            ((theme? 'gruvbox-dark-medium) "#70a038")
                            ((theme? 'eighties) "#a8f038")
                            (t "#4f8f3d"))))
    (face-spec-set 'parenthesis `((t (:foreground ,paren-color))))
    (defface square-brackets
      `((t (:foreground ,square-color)))
      'paren-face)
    (defface curly-brackets
      `((t (:foreground ,curly-color)))
      'paren-face)))

(defun jeff/init-ui (&optional frame initial _full)
  (interactive)
  (switch-custom-theme)
  ;; (scroll-bar-mode -1) (menu-bar-mode -1) (tool-bar-mode -1)
  (when (equal (system-name) "jeff-osx")
    (set-frame-width nil 100)
    (set-frame-height nil 48))
  (unless (laptop?)
    (global-display-line-numbers-mode 1))
  (powerline-reset)
  (powerline-default-theme)
  (when (and (gui-mac?) initial)
    (set-frame-fullscreen frame t))
  nil)

(provide 'init-theme)
