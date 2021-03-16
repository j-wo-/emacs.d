;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configure UI before display init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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
  "InputC3Mono Nerd Font 20"
  ;; "InputC3Mono Nerd Font 20"
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
;; (set-frame-font "InputC3Mono Nerd Font 19")
;; (set-frame-font (font-spec :family "Input Mono" :weight 'semi-bold :size 14))

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
        inhibit-message nil))

(defun --restore-temp-vars-post-init ()
  ;; (length file-name-handler-alist)
  ;; (length file-name-handler-alist-backup)
  (dolist (x file-name-handler-alist-backup)
    (add-to-list 'file-name-handler-alist x))
  (setq inhibit-message nil))

(--set-temp-vars-pre-init)
(add-hook 'after-init-hook #'--restore-temp-vars-post-init 95)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configure variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gc-cons-threshold-default gc-cons-threshold
      gc-cons-threshold (* 50 1024 1024)
      read-process-output-max (round (* 0.5 1024 1024))
      inhibit-splash-screen t
      make-backup-files nil
      custom-safe-themes t
      auto-save-default nil
      vc-follow-symlinks t
      echo-keystrokes 0.02
      minibuffer-message-timeout 1.0
      display-line-numbers-width-start 3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configure nativecomp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar --gcc-fast t)

(if --gcc-fast
    (setq comp-speed 3
          comp-native-driver-options
          ;; '("-march=native" "-O3")
          '("-march=native" "-Ofast" "-fno-finite-math-only")
          ;; "-g0"
          )
  (setq comp-speed 2
        comp-native-driver-options
        '("-march=native" "-O2")))

(setq comp-deferred-compilation-deny-list '("powerline"
                                            "slime"
                                            "pos-tip"
                                            "flycheck-pos-tip"
                                            "tooltip"
                                            "auto-margin"
                                            ;; "smartparens"
                                            ))

;; (setq load-no-native t comp-deferred-compilation nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
