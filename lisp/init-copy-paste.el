;;; -*- lexical-binding: t -*-

(require 'init-base)
(require 'cl-lib)
(require 'use-package)

;; This sets up terminal-mode Emacs instances to use the X shared clipboard
;; for kill and yank commands.
;;
;; Emacs needs to be started after the X server for this to work.

(defun xsel-paste ()
  (shell-command-to-string "xsel -ob"))

(defun xsel-copy (text &optional _push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "xsel -ib" "*Messages*" "xsel" "-ib")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun do-xsel-copy-paste-setup ()
  (when (and (not (mac?))
             (null window-system)
             (--window-system-available)
             (file-exists-p "/usr/bin/xsel")
             (not (equal (user-login-name) "root")))
    (setq interprogram-cut-function 'xsel-copy)
    (setq interprogram-paste-function 'xsel-paste)))

;;;
;;; copy/paste for Wayland
;;;

(defun wl-paste ()
  (shell-command-to-string "wl-paste -n"))

(defun wl-copy (text &optional _push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "wl-copy" "*Messages*" "wl-copy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun do-wayland-copy-paste-setup ()
  (when (and (--wayland-available)
             (null window-system)
             (file-exists-p "/usr/bin/wl-copy")
             (file-exists-p "/usr/bin/wl-paste")
             (not (equal (user-login-name) "root")))
    (setq interprogram-cut-function 'wl-copy)
    (setq interprogram-paste-function 'wl-paste)))

(defun jeff/init-copy-paste ()
  (interactive)
  (let ((inhibit-message t))
    (cond ((mac?)                nil)
          ((--wayland-available) (do-wayland-copy-paste-setup))
          (t                     (do-xsel-copy-paste-setup)))))

(provide 'init-copy-paste)
