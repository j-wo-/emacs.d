;;; -*- lexical-binding: t -*-

(require 'init-base)
(require 'use-package)

(global-auto-revert-mode t)
(transient-mark-mode t)
(delete-selection-mode t)

;; Add a hook to convert all tabs to spaces when saving any file,
;; unless its buffer mode is set to use tabs for indentation.
;;
;; (eg. makefile-gmake-mode will set indent-tabs-mode to t,
;;  so the syntactic tabs in Makefile files will be maintained)
(add-hook 'write-file-functions
          (lambda ()
            (cleanup-buffer)
            nil))
;;(setq write-file-functions nil)

(setq-default indent-tabs-mode nil)

;; find-function-C-source-directory
(when (mac?)
  (setq source-directory "~/Library/Caches/Homebrew/emacs-mac--git/"))

(windmove-default-keybindings '(shift))
(windmove-default-keybindings '(control meta))

;; ITERM2 MOUSE SUPPORT
(when (and (mac?) (null window-system))
  (require 'mwheel)
  (require 'mouse)
  (xterm-mouse-mode t)
  (mouse-wheel-mode t)
  (global-set-key [mouse-5] 'next-line)
  (global-set-key [mouse-4] 'previous-line))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

;; Make sure Emacs has the correct ssh-agent config,
;; in order to use tramp and git commands without requesting a password.
(unless (mac?)
  (if (equal (user-login-name) "root")
      (setenv "SSH_AUTH_SOCK" "/run/ssh-agent.socket")
    (setenv "SSH_AUTH_SOCK" (concat (getenv "XDG_RUNTIME_DIR") "/ssh-agent.socket"))))

;; Need to make sure emacs server daemon and emacsclient
;; are using the same path for the socket file.
;; The path is set here, and the same is set in a script
;; for starting emacsclient (/usr/local/bin/e).
(unless (graphical?)
  (setq server-socket-dir (format "/tmp/%s/emacs%d" (user-login-name) (user-uid))))

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode +1))

(use-package saveplace
  :ensure nil
  :config
  (setq save-place-file (expand-file-name "saveplace" --savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package savehist
  :ensure nil
  :config
  (setq savehist-additional-variables '(search-ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (expand-file-name "savehist" --savefile-dir))
  (savehist-mode +1))

(use-package recentf
  :disabled t
  :ensure nil
  :config
  (setq recentf-save-file (expand-file-name "recentf" --savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(provide 'init-configure)
