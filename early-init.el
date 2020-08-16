(set-language-environment "utf-8")

(setq file-name-handler-alist-backup file-name-handler-alist
      file-name-handler-alist nil
      gc-cons-threshold-default gc-cons-threshold
      gc-cons-threshold (* 100 1000 1000)
      read-process-output-max (* 1024 1024)
      inhibit-message nil
      inhibit-splash-screen t

      make-backup-files nil
      custom-safe-themes t
      auto-save-default nil
      vc-follow-symlinks t
      echo-keystrokes 0.1)

(setq comp-deferred-compilation nil
      comp-deferred-compilation-black-list '()
      comp-async-jobs-number 12
      comp-speed 3)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq default-frame-alist
      '((left-fringe . 10)
        (right-fringe . 10)
        ;; (internal-border-width . 4)
        (internal-border-width . 0)
        (font . "-FBI -InputMono Nerd Font-normal-normal-normal-*-26-*-*-*-*-0-iso10646-1")))

;;(set-face-attribute 'default nil :family "InputMono Nerd Font" :height 80 :weight 'normal)
;;(insert (concat "\n" (prin1-to-string (face-all-attributes 'default))))
;;(insert (concat "\n" (prin1-to-string (frame-parameters))))

;;(set-frame-font "InputMono Nerd Font:medium:pixelsize=26" nil t)

