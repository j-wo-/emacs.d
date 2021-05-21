;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configure UI before display init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

;; { 0, { "thin" }},
;; { 20, { "ultra-light", "ultralight" }},
;; { 40, { "extra-light", "extralight" }},
;; { 50, { "light" }},
;; { 75, { "semi-light", "semilight", "demilight", "book" }},
;; { 100, { "normal", "medium", "regular", "unspecified" }},
;; { 180, { "semi-bold", "semibold", "demibold", "demi" }},
;; { 200, { "bold" }},
;; { 205, { "extra-bold", "extrabold" }},
;; { 210, { "ultra-bold", "ultrabold", "black" }}

(defvar --default-font
  ;; "InputMono Nerd Font 17"
  ;; "InputC3Mono Nerd Font 20"
  ;; "InputC4Mono Nerd Font 20"
  "InputC4Mono Nerd Font 19"
  ;; "FiraCode Nerd Font:pixelsize=23"
  ;; "FuraCode Nerd Font:weight=Medium:pixelsize=23"
  ;; (font-spec :family "InputMono Nerd Font" :size 26)
  ;; (font-spec :family "Inconsolata" :weight 'semi-bold :size 18)
  ;; (font-spec :family "Inconsolata" :weight 'bold :size 18)
  ;; (font-spec :family "InputMono Nerd Font" :size 16)
  )
;; (find-font --default-font)
;; (font-xlfd-name --default-font)

;; (set-frame-font --default-font)
;; (set-frame-font (font-spec :family "Inconsolata" :weight 'bold :size 18))
;; (set-frame-font (font-spec :family "Inconsolata" :weight 'semi-bold :size 18))
;; (set-frame-font "InputMono Nerd Font 17")
;; (set-frame-font "InputC3Mono Nerd Font 20")
;; (set-frame-font "InputC4Mono Nerd Font 19")
;; (set-frame-font "InputC4Mono Nerd Font 19")
;; (set-frame-font "InputC4Mono Nerd Font:weight=Medium:pixelsize=25")
;; (set-frame-font (font-spec :family "Input Mono" :weight 'semi-bold :size 14))
;; (set-frame-font (font-spec :family "FuraCode Nerd Font" :weight 'medium :size 23))
;; (set-frame-font "FuraCode Nerd Font:weight=Medium:pixelsize=23")
;; (set-frame-font (font-spec :family "FiraCode Nerd Font" :size 23))

(setq default-frame-alist
      `((left-fringe . 12)
        (right-fringe . 12)
        (internal-border-width . 2)
        (font . ,(if (stringp --default-font)
                     --default-font
                   (font-xlfd-name --default-font))))
      warning-suppress-types '((comp)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set temporary values during emacs init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun --set-temp-vars-pre-init ()
  (defvar file-name-handler-alist-backup file-name-handler-alist)
  (setq file-name-handler-alist nil
        inhibit-message t))

(defun --restore-inhibit-message ()
  (setq inhibit-message nil))

(defun --restore-temp-vars-post-init ()
  ;; (length file-name-handler-alist)
  ;; (length file-name-handler-alist-backup)
  (dolist (x file-name-handler-alist-backup)
    (add-to-list 'file-name-handler-alist x))
  (run-with-timer 0.25 nil '--restore-inhibit-message))

(--set-temp-vars-pre-init)
(add-hook 'emacs-startup-hook #'--restore-temp-vars-post-init 95)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configure variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gc-cons-threshold-default gc-cons-threshold
      gc-cons-threshold (* 50 1000 1000)
      read-process-output-max (round (* 1.0 1000 1000))
      inhibit-splash-screen t
      make-backup-files nil
      custom-safe-themes t
      auto-save-default nil
      vc-follow-symlinks t
      echo-keystrokes 0.01
      minibuffer-message-timeout 1.0
      display-line-numbers-width-start 3
      load-prefer-newer t
      large-file-warning-threshold (* 100 1000 1000)
      confirm-kill-processes nil
      ;; ring-bell-function 'ignore
      ;;scroll-margin 0
      ;;scroll-conservatively 100000
      ;;scroll-preserve-screen-position t
      frame-title-format '((:eval
                            (if (buffer-file-name)
                                (abbreviate-file-name (buffer-file-name))
                              "%b")))
      require-final-newline t
      tab-always-indent 'complete)

(setq-default fill-column 90)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configure nativecomp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defvar --gcc-args '("-O2"))
(defvar --gcc-args '("-Ofast" "-fno-finite-math-only"))

(setq native-comp-speed 2
      native-comp-driver-options `("-march=native" "-g0" ,@--gcc-args)
      native-comp-deferred-compilation-deny-list '("powerline"
                                                   "slime"
                                                   "auto-margin"
                                                   "smartparens"))

'(setq load-no-native t
       native-comp-deferred-compilation nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
