;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configure UI before display init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(defvar --default-font
  (font-spec :family "InputMono Nerd Font" :size 26))
;; (find-font --default-font)
;; (font-xlfd-name --default-font)

(setq default-frame-alist
      `((left-fringe . 10)
        (right-fringe . 10)
        (internal-border-width . 0)
        ;;(font . ,(font-xlfd-name --default-font))
        (font . ,(font-xlfd-name --default-font))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set temporary values during emacs init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun --set-temp-vars-pre-init ()
  (setq file-name-handler-alist-backup file-name-handler-alist
        file-name-handler-alist nil
        inhibit-message t))

(defun --restore-temp-vars-post-init ()
  (setq file-name-handler-alist file-name-handler-alist-backup
        inhibit-message nil))

(--set-temp-vars-pre-init)
(add-hook 'after-init-hook '--restore-temp-vars-post-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configure variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gc-cons-threshold-default gc-cons-threshold
      gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (round (* 1.0 1024 1024))
      inhibit-splash-screen t
      make-backup-files nil
      custom-safe-themes t
      auto-save-default nil
      vc-follow-symlinks t
      echo-keystrokes 0.01)
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
