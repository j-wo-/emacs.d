;;; -*- lexical-binding: t -*-

(require 'init-base)
(require 'init-main)

(when jeff/enable-auto-neotree
  (load-local 'init-neotree t))

(defun --elapsed-alert (title elapsed)
  (let ((alert-fade-time 3))
    (alert (format "Elapsed: %.3fs" elapsed)
           :title title)))

(defmacro --with-elapsed-time-alert (&rest body)
  `(--elapsed-alert (--body-title ',body)
                    (--with-elapsed-time ,@body)))
;;(--with-elapsed-time-alert (+ 1 2))

(defun jeff/describe-init ()
  (interactive)
  (let ((alert-fade-time (if (display-graphic-p) 4 2)))
    (alert (format "Emacs started in %s" (jeff/init-time t))
           :title (format "Emacs <%s>" (buffer-name))
           :category 'emacs-init)))

(defun jeff/after-init ()
  (jeff/init-ui (selected-frame) t)
  (jeff/init-copy-paste)
  (if (graphical?)
      (jeff/describe-init)
    (run-with-timer 0.33 nil 'jeff/describe-init))
  (garbage-collect))

(add-hook 'emacs-startup-hook 'jeff/after-init)
;;(add-hook 'after-make-frame-functions 'jeff/init-ui)
;;(add-hook 'after-make-frame-functions 'jeff/init-copy-paste)

;;(add-hook 'post-command-hook '--force-minibuffer-update)
;;(add-hook 'post-command-hook 'force-mode-line-update)
;;(setq post-command-hook (delete 'force-mode-line-update post-command-hook))

(provide 'init-post)
