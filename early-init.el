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
  (font-spec :family "InputMono Nerd Font" :size 15)
  ;; (font-spec :family "Inconsolata" :weight 'semi-bold :size 18)
  ;; (font-spec :family "Inconsolata" :weight 'bold :size 18)
  ;; (font-spec :family "InputMono Nerd Font" :size 16)
  )
;; (find-font --default-font)
;; (font-xlfd-name --default-font)

;; (set-frame-font --default-font)
;; (set-frame-font (font-spec :family "Inconsolata" :weight 'bold :size 18))
;; (set-frame-font (font-spec :family "Inconsolata" :weight 'semi-bold :size 18))
;; (set-frame-font (font-spec :family "InputMono Nerd Font" :size 15))
;; (set-frame-font (font-spec :family "Input Mono" :weight 'semi-bold :size 14))

(setq default-frame-alist
      `((left-fringe . 8)
        (right-fringe . 8)
        ;;(internal-border-width . 0)
        ;;(font . ,(font-xlfd-name --default-font))
        (font . ,(font-xlfd-name --default-font))))

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
      read-process-output-max (round (* 1.0 1024 1024))
      inhibit-splash-screen t
      make-backup-files nil
      custom-safe-themes t
      auto-save-default nil
      vc-follow-symlinks t
      echo-keystrokes 0.02)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configure nativecomp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq comp-speed 3
      ;; comp-async-jobs-number 12
      ;; comp-deferred-compilation nil
      comp-deferred-compilation-black-list '("powerline" "slime"))
(setq comp-native-driver-options
      '("-march=native" "-Ofast" "-g0" "-fno-finite-math-only"))
;; (setq comp-always-compile t)
;; (setq load-no-native t comp-deferred-compilation nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
