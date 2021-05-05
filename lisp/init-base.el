;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'init-use-package)
(require 'use-package)
(use-package dash)
(defvar --savefile-dir (concat user-emacs-directory "save/"))

(defcustom jeff/enable-auto-neotree nil
  "Non-nil enables hooks to integrate neotree into various actions."
  :group 'jeff
  :type 'boolean)

(defcustom jeff/use-spaceline nil
  "Uses spaceline for modeline if non-nil."
  :group 'jeff
  :type 'boolean)

(defvar --exclude-pkgs
  '(evil auto-complete --company))

(defun exclude-pkg? (pkg)
  (member pkg --exclude-pkgs))

(defun --window-system-available () (< 0 (length (getenv "DISPLAY"))))
(defun --wayland-available () (< 0 (length (getenv "WAYLAND_DISPLAY"))))
(defun graphical? () (cl-some #'display-graphic-p (frame-list)))
(defun laptop? () (or (equal (system-name) "jeff-mbp")
                      (equal (system-name) "jeff-laptop")
                      (equal (system-name) "jeff-m1.lan")))
(defun mac? () (eql system-type 'darwin))
(defun gui-mac-std? () (eql window-system 'ns))
(defun gui-emacs-mac? () (eql window-system 'mac))
(defun gui-mac? () (or (gui-mac-std?) (gui-emacs-mac?)))

(defun symbol-matches (sym str)
  (not (null (string-match-p str (symbol-name sym)))))

(defmacro define-map-keys (map &rest defs)
  `(progn ,@(mapcar (lambda (entry)
                      (cl-destructuring-bind (kbd-str func) entry
                        `(define-key ,map (kbd ,kbd-str) ,func)))
                    defs)))

(defmacro define-map-keys-multi (maps &rest defs)
  `(progn ,@(mapcar (lambda (m) `(define-map-keys ,m ,@defs))
                    maps)))

(fset 'yes-or-no-p 'y-or-n-p)

(defun --indent-tabs-on ()
  (setq-local indent-tabs-mode t))
(defun --indent-tabs-off ()
  (setq-local indent-tabs-mode t))
(defun --indent-tabs-mode (mode-hook enable-tabs)
  (add-hook mode-hook (if enable-tabs '--indent-tabs-on '--indent-tabs-off)))

(defvar cleanup-buffer-enable t)

(defun cleanup-buffer-toggle ()
  (interactive)
  (if cleanup-buffer-enable
      (progn
        (setq cleanup-buffer-enable nil)
        (message "cleanup-buffer disabled"))
    (progn
      (setq cleanup-buffer-enable t)
      (message "cleanup-buffer enabled"))))

(defun cleanup-buffer ()
  (interactive)
  (when cleanup-buffer-enable
    (when (not indent-tabs-mode)
      (untabify (point-min) (point-max)))
    (delete-trailing-whitespace (point-min) (point-max))))

(defmacro load-local (feature &optional no-compile)
  `(let* (;; (no-compile t)
          (no-compile ,no-compile)
          (file (symbol-name ,feature))
          (path (locate-user-emacs-file (concat "lisp/" file)))
          (ext (if no-compile ".el" ".elc"))
          (no-byte-compile no-compile))
     (require ,feature)
     (unless no-compile
       (byte-recompile-file (concat path ".el") nil 0 nil))
     ;;(load (concat path ext) nil t t)
     (load (concat path ext) nil nil t)
     (require ,feature)))

;;(byte-recompile-file "~/.emacs.d/lisp/init-main.el" t 0 nil)

(eval-and-compile
  (defun symbol-suffix (sym suffix)
    (intern (concat (symbol-name sym) suffix))))

(defmacro set-mode-name (mode name)
  (let ((func-name (intern (concat "--set-mode-name--" (symbol-name mode)))))
    `(progn
       (defun ,func-name () (setq mode-name ,name))
       (add-hook ',(symbol-suffix mode "-hook") #',func-name 100)
       (when (eql major-mode ',mode)
         (,func-name)))))

(defun call-stack ()
  "Return the current call stack frames."
  (let ((frames)
        (frame)
        (index 5))
    (while (setq frame (backtrace-frame index))
      (push frame frames)
      (cl-incf index))
    (cl-remove-if-not 'car frames)))

(defun function-stack ()
  "Like call-stack but is a list of only the function names"
  (butlast (mapcar 'cl-second (call-stack))))

(defun set-frame-fullscreen (frame active)
  (let ((current (frame-parameter (or frame (selected-frame)) 'fullscreen)))
    (when (or (and active (not current))
              (and current (not active)))
      (toggle-frame-fullscreen frame))))

(defun print-to-buffer (x)
  (princ (concat "\n" (with-output-to-string (print x))) (current-buffer)))

(defun active-minor-modes ()
  (--filter (and (boundp it) (symbol-value it)) minor-mode-list))

;;(print-to-buffer (active-minor-modes))

(defun minor-mode-active-p (minor-mode)
  (if (member minor-mode (active-minor-modes)) t nil))

(defun --force-minibuffer-update ()
  (force-window-update (minibuffer-window)))

(eval-and-compile
  (defun native-comp? ()
    (and (functionp 'native-comp-available-p)
         (native-comp-available-p))))

(defmacro --when-native-comp (&rest body)
  (when (native-comp?)
    `(progn ,@body)))

(defun jeff/native-comp-path (path)
  (--when-native-comp
   (let ((comp-always-compile t))
     (native-compile-async path t nil))))

(defun jeff/native-comp-elpa ()
  (interactive)
  (jeff/native-comp-path "~/.emacs.d/elpa/")
  t)

(defun jeff/native-comp-emacs-base ()
  (interactive)
  (jeff/native-comp-path "/usr/local/share/emacs/28.0.50/lisp/")
  t)

(defun jeff/native-comp-all ()
  (interactive)
  (jeff/native-comp-emacs-base)
  (jeff/native-comp-elpa)
  t)

(defun --body-title (body)
  (let* ((form (car (last body)))
         (full (prin1-to-string form)))
    (cond ((<= (length full) 40)
           full)
          ((and (listp form) (symbolp (car form)))
           (format "(%s ...)" (symbol-name (car form))))
          (t "<body>"))))

(defun --elapsed-seconds (start-time)
  (time-to-seconds (time-since start-time)))

(defmacro --with-elapsed-time (&rest body)
  `(let ((time-start (current-time)))
     ,@body
     (let ((elapsed (time-since time-start)))
       (time-to-seconds elapsed))))

(defun jeff/init-time (&optional as-string)
  (let ((init-time (float-time
                    (time-subtract after-init-time before-init-time))))
    (if as-string (format "%.2fs" init-time) init-time)))
;;(jeff/init-time t)

(provide 'init-base)
