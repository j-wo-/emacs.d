(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq gc-cons-threshold-default gc-cons-threshold
      gc-cons-threshold (* 100 1000 1000)
      file-name-handler-alist-backup file-name-handler-alist
      file-name-handler-alist nil
      ;; read-process-output-max (* 1024 1024)
      read-process-output-max (* 1024 256)
      inhibit-message t
      inhibit-splash-screen t

      make-backup-files nil
      custom-safe-themes t
      auto-save-default nil
      vc-follow-symlinks t
      echo-keystrokes 0.1)

(setq comp-async-jobs-number 12
      comp-speed 3
      comp-deferred-compilation t
      comp-deferred-compilation-black-list '("powerline" "slime" "auto-cider")
      ;; load-no-native nil
      ;; comp-always-compile t
      )

(setq default-frame-alist
      '((left-fringe . 10)
        (right-fringe . 10)
        (internal-border-width . 0)
        (font . "-FBI -InputMono Nerd Font-normal-normal-normal-*-26-*-*-*-*-0-iso10646-1")))

;;(set-frame-font "InputMono Nerd Font:medium:pixelsize=26" nil t)
