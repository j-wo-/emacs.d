;;; -*- lexical-binding: t -*-

(require 'init-base)
(require 'use-package)

(defvar jeff/all-config-files
  (list "~/.emacs.d/init.el"
        "~/.emacs.d/early-init.el"
        "~/.gitconfig"
        "~/.ssh/config"
        "~/.config/mpv/mpv.conf"
        "~/.config/sway/config"
        "~/.config/mako/config"
        "~/.config/waybar/config"
        "~/.config/wofi/config"
        "~/.config/alacritty/alacritty.yml"
        "~/.config/systemd/user/compile.slice"
        "~/.config/systemd/user/sway-session.target"
        "~/bin/launch-sway-programs"
        "~/bin/makepkg-chroot"
        "~/abs/build"
        "~/abs/build-one"
        "~/abs/build-all"))

(defun --launch-user-main ()
  (interactive)
  (with-delay 0.5
    (delete-other-windows)
    (dolist (f (append jeff/all-config-files '("~/.emacs.d/init.el")))
      (message "opened: %s" f)
      (find-file-existing f))
    (when (and nil (graphical?))
      (split-window-right)
      (switch-to-buffer-other-window (messages-buffer)))))

(defun --launch-todo ()
  (interactive)
  (use-package org)
  (with-delay 0.5
    (find-file "~/.emacs.d/init.el")
    (find-file "~/org/self.org")
    (delete-other-windows)
    (org-shifttab)
    (org-shifttab)
    (org-shifttab)
    (split-window-right)
    (org-agenda-list)
    (jeff/load-org-notify)
    (org-notify-enable)))

(defun --launch-code ()
  (interactive)
  (use-package clojure-mode)
  (use-package magit)
  (with-delay 0.5
    (find-file "~/code/sysrev/project.clj")
    (split-window-right)
    (magit-status-internal default-directory)
    (sysrev)))

(provide 'init-launch)
