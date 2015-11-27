;;(package-initialize)

(let ((file-name-handler-alist nil))
  
  (setq gc-cons-threshold 10000000)

  (set-language-environment "utf-8")

  (setq custom-safe-themes t)
  (setq auto-save-default nil)

  (require 'cask "~/.cask/cask.el")
  (cask-initialize)

  (require 'cl)
  (require 'use-package)
  (require 'f)

  (defun load-local (file)
    (load (f-expand file user-emacs-directory)))

  ;; load values for options that may be different across machines
  (load-local "variables")

  (use-package diminish)

  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  (setq vc-follow-symlinks t)
  (setq make-backup-files nil)

  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)

  (unless (null window-system)
    (set-frame-size (selected-frame) custom-frame-width custom-frame-height))

  (unless (null window-system)
    (set-frame-font custom-font))

  '(let ((blue-bg "#1d64c9")
         (white-bg "#bfbfbf")
         (white-fg "#eaeaea")
         (black-fg "#181818")
         (gray-fg "#b0b0b0"))
     (custom-set-faces
      `(mode-line ((t (:foreground ,white-fg :background ,blue-bg))))
      `(mode-line-inactive ((t (:foreground ,gray-fg :background ,blue-bg))))
      `(mode-line-buffer-id ((t (:foreground ,white-fg :background ,blue-bg))))
      `(powerline-active1 ((t (:foreground ,black-fg :background ,white-bg))))
      `(powerline-active2 ((t (:foreground ,white-fg :background ,blue-bg))))
      `(powerline-inactive1 ((t (:foreground ,gray-fg :background ,black-fg))))
      `(powerline-inactive2 ((t (:foreground ,gray-fg :background ,blue-bg))))))

  
  (use-package powerline
    :config
    ;;(powerline-default-theme)
    )

  (defun switch-to-theme (theme)
    (dolist (active-theme custom-enabled-themes)
      (disable-theme active-theme))
    (when theme
      (load-theme theme t)))

  ;;(use-package zenburn-theme)
  (use-package color-theme-sanityinc-tomorrow)
  (use-package color-theme-sanityinc-solarized)
  (use-package moe-theme)
  (use-package base16-theme)
  (use-package gruvbox-theme)

  (defvar custom-emacs-theme)
  (setq custom-emacs-theme
        (if (null window-system)
            'gruvbox
          'gruvbox))

  (cond
   ((or (eql custom-emacs-theme 'moe-dark)
        (eql custom-emacs-theme 'moe-light))
    (progn
      (if (eql custom-emacs-theme 'moe-dark)
          (moe-dark)
        (moe-light))
      (powerline-moe-theme)))
   (t
    (progn
      (switch-to-theme custom-emacs-theme)
      (powerline-default-theme))))
  
  (unless (null window-system)
    (set-frame-size (selected-frame) 100 58))

  (use-package uniquify
    :config (setq uniquify-buffer-name-style 'post-forward))

  (use-package aggressive-indent)

  (use-package lispy
    :config 
    (defun enable-lispy (mode-hook)
      (add-hook mode-hook (lambda () (lispy-mode 1)))))

  (use-package systemd)

  (use-package org
    :config
    (define-key global-map "\C-cl" 'org-store-link)
    (define-key global-map "\C-ca" 'org-agenda)
    (define-key global-map "\C-cc" 'org-capture)
    (setq org-log-done t)
    (setq org-agenda-files (list "~/org/work.org" "~/org/self.org")))

  (use-package pkgbuild-mode
    :config
    (add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode)))

  (use-package auto-complete-config
    :config
    (ac-config-default)
    (setq global-auto-complete-mode t)
    (setq ac-use-dictionary-as-stop-words nil)
    (setq ac-use-fuzzy t)
    (define-globalized-minor-mode real-global-auto-complete-mode
      auto-complete-mode (lambda ()
                           (if (not (minibufferp (current-buffer)))
                               (auto-complete-mode 1))))
    (real-global-auto-complete-mode t))

  (use-package projectile
    :config
    (projectile-global-mode)
    (setq projectile-enable-caching t)
    (setq projectile-mode-line
          '(:eval (format " [%s]" (projectile-project-name)))))

  (use-package smex
    :bind ("M-x" . smex)
    :config (smex-initialize))

  (use-package js2-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
    (let ((js-hook
           (lambda ()
             (setf tab-width 4)
             (setf indent-tabs-mode nil))))
      (add-hook 'js2-mode-hook js-hook)
      (add-hook 'js2-jsx-mode-hook js-hook)))

  (use-package scala-mode2
    :mode
    ("\\.scala\\'" . scala-mode)
    ("\\.sbt\\'" . scala-mode)
    :init
    :config
    ;;(setq scala-indent:default-run-on-strategy 1)
    ;;(setq scala-indent:indent-value-expression nil)
    (use-package ensime
      :config
      ;;(setq ensime-completion-style 'auto-complete)
      (setq ensime-auto-generate-config t)
      (setq ensime-typecheck-idle-interval 0.3)
      (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
      (add-hook 'scala-mode-hook (lambda () (auto-complete-mode -1)))
      '(add-hook 'scala-mode-hook
                 (lambda ()
                   (add-hook 'post-command-hook
                             (lambda ()
                               (when (and ensime-mode (ensime-connected-p))
                                 (ensime-print-errors-at-point)))
                             t t)))
      (define-key scala-mode-map "\C-t" 'ensime-print-type-at-point)
      (define-key scala-mode-map "\C-\M-e" 'ensime-print-errors-at-point)
      (define-key scala-mode-map "\C-c." 'ensime-forward-note)
      (define-key scala-mode-map "\C-c," 'ensime-backward-note)))
  
  (use-package ido
    :config
    (ido-mode 1)
    (setq ido-use-faces nil)
    (use-package flx-ido :config (flx-ido-mode 1))
    (use-package ido-ubiquitous :config (ido-ubiquitous-mode)))

  (use-package mic-paren :config (paren-activate))

  (use-package paren-face :config (global-paren-face-mode))

  (use-package paredit
    :config
    (define-globalized-minor-mode real-global-paredit-mode
      paredit-mode (lambda ()
                     (if (not (minibufferp (current-buffer)))
                         (enable-paredit-mode))))
    ;;(real-global-paredit-mode t)
    (diminish 'paredit-mode "()"))

  (use-package smartparens-config
    :config
    (use-package smartparens)
    (sp-pair "'" nil :actions :rem)
    (sp-pair "`" nil :actions :rem)
    (smartparens-global-mode t))

  (add-hook
   'emacs-lisp-mode-hook
   (lambda ()
     (use-package ielm)
     (use-package elisp-slime-nav
       :config (diminish 'elisp-slime-nav-mode "M-."))
     (turn-on-elisp-slime-nav-mode)
     (aggressive-indent-mode)
     (turn-off-smartparens-mode)
     (enable-paredit-mode)))
  ;; (enable-lispy 'emacs-lisp-mode-hook)

  (use-package clojure-mode
    :mode "\\.clj\\'" "\\.cljs\\'"
    :config
    (defface square-brackets
      '((t (:foreground "#c0c43b"))) 'paren-face)
    (defface curly-brackets
      '((t (:foreground "#50a838"))) 'paren-face)
    (defconst clojure-brackets-keywords
      '(("\\[" 0 'square-brackets)
        ("\\]" 0 'square-brackets)
        ("[\\{\\}]" 0 'curly-brackets)))
    (add-hook
     'paren-face-mode-hook
     (lambda ()
       (if paren-face-mode
           (font-lock-add-keywords nil clojure-brackets-keywords t)
         (font-lock-remove-keywords nil clojure-brackets-keywords))
       (when (called-interactively-p 'any)
         (font-lock-fontify-buffer))))
    (use-package cider
      :config
      ;;(setq cider-cljs-repl "(cemerick.piggieback/cljs-repl (cljs.repl.rhino/repl-env))")
      (setq cider-cljs-repl "(do (require 'cljs.repl.node) (cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env)))")
      ;;(setq cider-cljs-repl "(do (require 'weasel.repl.websocket) (cemerick.piggieback/cljs-repl (weasel.repl.websocket/repl-env :ip \"127.0.0.1\" :port 9001)))")
      (use-package ac-cider
        :config
        (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
        (add-hook 'cider-mode-hook 'ac-cider-setup)
        (add-hook 'cider-repl-mode-hook 'ac-cider-setup))
      (use-package cider-eldoc
        :config (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))
      (add-hook 'clojure-mode-hook #'cider-mode)
      (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
      (add-hook 'cider-mode-hook #'aggressive-indent-mode)
      (add-hook 'clojure-mode-hook 'turn-off-smartparens-mode)
      (add-hook 'cider-mode-hook 'turn-off-smartparens-mode)
      (add-hook 'cider-repl-mode-hook 'turn-off-smartparens-mode)
      (add-hook 'clojure-mode-hook 'enable-paredit-mode)
      (add-hook 'cider-mode-hook 'enable-paredit-mode)
      (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
      ;; (enable-lispy 'clojure-mode-hook)
      ;; (enable-lispy 'cider-mode-hook)
      ;; (enable-lispy 'cider-repl-mode-hook)
      (setq cider-lein-command "~/bin/lein")
      (setq cider-repl-popup-stacktraces t)
      (setq cider-auto-select-error-buffer t)))

  (use-package haskell-mode
    :mode "\\.hs\\'" "\\.hs-boot\\'" "\\.lhs\\'" "\\.lhs-boot\\'"
    :config
    (use-package ghc)
    (use-package ac-haskell-process
      :config
      (add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
      (add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)
      (add-to-list 'ac-modes 'haskell-interactive-mode))
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
    (autoload 'ghc-init "ghc" nil t)
    (autoload 'ghc-debug "ghc" nil t)
    (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
    (setq haskell-process-suggest-remove-import-lines t)
    (setq haskell-process-auto-import-loaded-modules t)
    (setq haskell-process-log t)
    (setq haskell-process-type 'cabal-repl))

  (add-hook 'java-mode-hook (lambda () (setq indent-tabs-mode nil)))

  (use-package tramp
    :config
    (setq tramp-default-method "ssh"))

  (use-package slime-autoloads
    :commands slime
    :mode
    ("\\.lisp\\'" . lisp-mode)
    ("\\.asd\\'" . lisp-mode)
    :config
    (setq slime-contribs '(slime-fancy slime-tramp))
    (use-package slime
      :config
      (add-hook 'lisp-mode-hook
                (lambda ()
                  (setq-local lisp-indent-function
                              'common-lisp-indent-function)))
      
      (defvar sbcl-run-command "sbcl --dynamic-space-size 2000 --noinform")
      (defvar ccl-run-command "ccl64 -K utf-8")
      (defvar ecl-run-command "ecl")
      
      (setq inferior-lisp-program sbcl-run-command
            slime-net-coding-system 'utf-8-unix
            slime-complete-symbol-function 'slime-fuzzy-complete-symbol
            slime-fuzzy-completion-in-place t
            slime-enable-evaluate-in-emacs t
            slime-autodoc-use-multiline-p t
            slime-load-failed-fasl 'never
            slime-compile-file-options
            '(:fasl-directory "/tmp/slime-fasls/"))
      (make-directory "/tmp/slime-fasls/" t)
      ;;(define-key slime-mode-map [(return)] 'paredit-newline)
      (define-key slime-mode-map (kbd "C-c .") 'slime-next-note)
      (define-key slime-mode-map (kbd "C-c ,") 'slime-previous-note)

      (defun slime-sbcl ()
        (interactive)
        (setq inferior-lisp-program sbcl-run-command)
        (slime))
      (defun slime-ccl ()
        (interactive)
        (setq inferior-lisp-program ccl-run-command)
        (slime))
      (defun slime-ecl ()
        (interactive)
        (setq inferior-lisp-program ecl-run-command)
        (slime))
      (defun kill-slime ()
        (interactive)
        (kill-buffer "*inferior-lisp*")
        (cond
         ((equal inferior-lisp-program sbcl-run-command)
          (kill-buffer "*slime-repl sbcl*"))
         ((equal inferior-lisp-program ccl-run-command)
          (kill-buffer "*slime-repl ccl*"))))

      (add-to-list 'projectile-globally-ignored-modes "comint-mode")
      (add-to-list 'projectile-globally-ignored-modes "slime-repl-mode")

      (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
      (add-hook 'slime-mode-hook #'aggressive-indent-mode)
      (add-hook 'lisp-mode-hook 'turn-off-smartparens-mode)
      (add-hook 'slime-mode-hook 'turn-off-smartparens-mode)
      (add-hook 'slime-repl-mode-hook 'turn-off-smartparens-mode)
      (add-hook 'lisp-mode-hook 'enable-paredit-mode)
      (add-hook 'slime-mode-hook 'enable-paredit-mode)
      (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
      
      ;; (enable-lispy 'lisp-mode-hook)
      ;; (enable-lispy 'slime-mode-hook)
      ;; (enable-lispy 'slime-repl-mode-hook)

      (use-package slime-annot)
      (use-package ac-slime
        :config
        (defun set-up-slime-ac-fuzzy ()
          (set-up-slime-ac t))
        (add-hook 'lisp-mode-hook 'set-up-slime-ac-fuzzy)
        (add-hook 'slime-repl-mode-hook 'set-up-slime-ac-fuzzy))))

  (if (null (window-system))
      (require 'git-gutter)
    (require 'git-gutter-fringe))
  (global-git-gutter-mode t)
  (diminish 'git-gutter-mode)

  (use-package magit
    :init (setq magit-last-seen-setup-instructions "1.4.0"))

  (defun xsel-paste ()
    (shell-command-to-string "xsel -ob"))

  (defun xsel-copy (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "xsel -ib" "*Messages*" "xsel" "-ib")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (when (and (null window-system)
             (getenv "DISPLAY")
             (file-exists-p "/usr/bin/xsel")
             (not (equal (user-login-name) "root")))
    (setq interprogram-cut-function 'xsel-copy)
    (setq interprogram-paste-function 'xsel-paste))

  (load-local "keys")
  (load-local "commands")

  (setq server-socket-dir
        (format "/tmp/%s/emacs%d" (user-login-name) (user-uid))))
