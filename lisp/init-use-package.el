;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'package)

(unless (and t (>= emacs-major-version 27))
  (package-initialize))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(defun pin-pkg (pkg archive)
  (add-to-list 'package-pinned-packages (cons pkg archive) t))

(defun unpin-pkg (pkg)
  (setq package-pinned-packages
        (cl-remove-if (lambda (x) (eql (car x) pkg))
                      package-pinned-packages)))

;;;
;;; use-package bootstrapping (support automatic bootstrap from empty elpa library)
;;;

(pin-pkg 'use-package "melpa")

;; Install use-package from melpa if needed
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(defmacro ensure-installed (pin &rest pkgs)
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (pkg)
                 (when (not (package-installed-p pkg))
                   `(progn
                      (setq inhibit-message nil)
                      (message "ensure-installed [%s] ..." (symbol-name ',pkg))
                      (use-package ,pkg :pin ,pin))))
               pkgs)))

;; This is to get around an infinite recursion in emacs-lisp-mode-hook
;; when bootstrapping packages.
(ensure-installed melpa-stable
  paren-face
  elisp-slime-nav
  paredit)

(provide 'init-use-package)
