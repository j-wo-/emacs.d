(setq
 ;; startup time optimization
 ;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
 file-name-handler-alist-backup file-name-handler-alist
 file-name-handler-alist nil
 gc-cons-threshold-default gc-cons-threshold
 gc-cons-threshold (* 100 1000 1000)
 ;; prevent echoing messages while loading
 ;; inhibit-message t
 inhibit-splash-screen t)

(defun restore-config-post-init ()
  (setq inhibit-message nil
        file-name-handler-alist file-name-handler-alist-backup)
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setq gc-cons-threshold (* 20 1000 1000)))))
(add-hook 'after-init-hook 'restore-config-post-init)

(require 'cl)

(defun graphical? () (some #'display-graphic-p (frame-list)))
(defun laptop? () (or (equal system-name "jeff-mbp")
                      (equal system-name "jeff-laptop")))
(defun mac? () (eql system-type 'darwin))
(defun gui-mac-std? () (eql window-system 'ns))
(defun gui-emacs-mac? () (eql window-system 'mac))
(defun gui-mac? () (or (gui-mac-std?) (gui-emacs-mac?)))

(defvar custom-emacs-theme 'zenburn)
;; 'gruvbox-dark-hard 'flatland
;; (if (graphical?) 'sanityinc-tomorrow-night 'sanityinc-tomorrow-night-rxvt)

(set-language-environment "utf-8")

(setq default-frame-alist '((left-fringe . 12) (right-fringe . 12))
      custom-safe-themes t
      auto-save-default nil
      vc-follow-symlinks t
      make-backup-files nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(global-auto-revert-mode t)

(defun symbol-matches (sym str)
  (not (null (string-match-p str (symbol-name sym)))))

;;;
;;; Package setup
;;;

(require 'package)

(unless (>= emacs-major-version 27)
  (package-initialize))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(defun pin-stable (pkg)
  (add-to-list 'package-pinned-packages (cons pkg "melpa-stable") t))

(defun unpin-pkg (pkg)
  (setq package-pinned-packages
        (remove-if (lambda (x)
                     (eql (first x) pkg))
                   package-pinned-packages)))

(dolist (pkg '(web-mode magit git-commit js2-mode tern slime company
                        --markdown-mode cider clojure-mode clj-refactor))
  (unless (symbol-matches pkg "^--")
    (pin-stable pkg)))

;;;
;;; use-package bootstrapping (support automatic bootstrap from empty elpa library)
;;;

;; Install use-package from melpa if needed
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(defmacro ensure-installed (&rest pkgs)
  `(progn
     ,@(mapcar (lambda (pkg)
                 `(when (not (package-installed-p ',pkg))
                    (use-package ,pkg)))
               pkgs)))

;; This is to get around an infinite recursion in emacs-lisp-mode-hook
;; when bootstrapping packages.
(ensure-installed paren-face elisp-slime-nav paredit aggressive-indent)

(require 'use-package)
(setq use-package-always-ensure t)

;; Add a hook to convert all tabs to spaces when saving any file,
;; unless its buffer mode is set to use tabs for indentation.
;;
;; (eg. makefile-gmake-mode will set indent-tabs-mode to t,
;;  so the syntactic tabs in Makefile files will be maintained)
(add-hook 'write-file-hooks
          (lambda ()
            (when (not indent-tabs-mode)
              (untabify (point-min) (point-max))
              nil)))

(defun active-minor-modes ()
  (--filter (and (boundp it) (symbol-value it)) minor-mode-list))
(defun minor-mode-active-p (minor-mode)
  (if (member minor-mode (active-minor-modes)) t nil))

(defun load-local (file)
  (let ((inhibit-message t))
    (load (locate-user-emacs-file file))))

(load-local "keys")
(load-local "commands")

(windmove-default-keybindings)

;;;
;;; load packages
;;;

(defvar jeffwk/exclude-pkgs
  ;; '(evil company)
  '(evil auto-complete))

(defun exclude-pkg? (pkg)
  (if (member pkg jeffwk/exclude-pkgs) t nil))

(use-package dash)

(use-package diminish
  :config (diminish 'eldoc-mode))

(use-package evil
  :if (not (exclude-pkg? 'evil))
  :config (evil-mode 1))

;;;
;;; general
;;;

(use-package disable-mouse
  :if (laptop?)
  :defer 0.25
  :config
  (diminish 'global-disable-mouse-mode)
  (diminish 'disable-mouse-mode)
  (diminish 'disable-mouse-global-mode)
  (disable-mouse-global-mode))

(defun --tramp-config ()
  (setq tramp-default-method "ssh"))

(defvar --tramp-use-package t)

(defun ensure-tramp ()
  (if --tramp-use-package
      (use-package tramp)
    (unless (member 'tramp features)
      (require 'tramp)
      (funcall #'--tramp-config))))

(use-package tramp
  :if --tramp-use-package
  :defer t
  :config (funcall #'--tramp-config))

(use-package rainbow-mode)

(use-package helm
  :defer t
  :init (setq helm-follow-mode-persistent t)
  :config
  (require 'helm-buffers)
  (dolist (b '("\\`\\*esup" "\\`\\*Warnings" "\\`\\*Help" "\\`\\*magit-"
               "\\`\\*cider-\\*" "\\`\\*tramp" "\\`\\*nrepl" "\\`*Compile"))
    (add-to-list 'helm-boring-buffer-regexp-list b))
  (dolist (b '("\\`\\*magit\\: " "\\`\\*cider-repl "))
    (add-to-list 'helm-white-buffer-regexp-list b))
  (defun jeffwk/helm-buffers-list-all ()
    (interactive)
    (let ((helm-boring-buffer-regexp-list
           '("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf")))
      (helm-buffers-list)))
  (define-key global-map (kbd "C-x B") 'jeffwk/helm-buffers-list-all)
  ;; helm-map
  ;; helm-ag-map
  ;; helm-do-ag-map
  (use-package helm-ag
    :init (setq helm-ag-insert-at-point 'symbol
                helm-ag-use-temp-buffer t)))

(use-package esup :commands esup)

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(defun call-stack ()
  "Return the current call stack frames."
  (let ((frames)
        (frame)
        (index 5))
    (while (setq frame (backtrace-frame index))
      (push frame frames)
      (incf index))
    (remove-if-not 'car frames)))

(defun function-stack ()
  "Like call-stack but is a list of only the function names"
  (butlast (mapcar 'cl-second (call-stack))))

(defvar use-global-aggressive-indent t)

(use-package aggressive-indent
  :config
  '(setq aggressive-indent-sit-for-time 0.05)
  '(setq aggressive-indent-dont-indent-if
         (list (lambda ()
                 (let ((active-fnames (function-stack)))
                   (some (lambda (fname)
                           (member fname active-fnames))
                         '(company-capf
                           completion-all-completions
                           all-completions
                           cider-complete
                           nrepl-send-sync-request
                           cider-sync-request:complete
                           accept-process-output))))))
  (when use-global-aggressive-indent
    ;; aggressive-indent-excluded-modes
    (dolist (mode '(cider-repl-mode))
      (add-to-list 'aggressive-indent-excluded-modes mode))
    '(dolist (mode '(clojure-mode clojurescript-mode cider-mode cider-repl-mode))
       '(setq aggressive-indent-excluded-modes
              (remove mode aggressive-indent-excluded-modes))
       (add-to-list 'aggressive-indent-excluded-modes mode))
    (aggressive-indent-global-mode)))

(use-package auto-complete
  :if (not (exclude-pkg? 'auto-complete))
  :defer 0.25
  :config
  (ac-config-default)
  (setq ac-delay 0.025
        ac-max-width 50
        ac-auto-show-menu 0.4
        ac-quick-help-delay 0.8
        global-auto-complete-mode t
        ac-use-dictionary-as-stop-words nil
        ac-use-fuzzy t)
  (define-globalized-minor-mode real-global-auto-complete-mode
    auto-complete-mode (lambda ()
                         (if (not (minibufferp (current-buffer)))
                             (auto-complete-mode 1))))
  (real-global-auto-complete-mode t))

(use-package company
  :if (not (exclude-pkg? 'company))
  :diminish company-mode global-company-mode
  :init
  ;; http://emacs.stackexchange.com/a/10838/12585
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        ;; company-tooltip-offset-display 'scrollbar
        company-tooltip-offset-display 'lines
        company-minimum-prefix-length 3
        company-idle-delay 0.15)
  :config
  (add-to-list 'company-transformers 'company-sort-by-occurrence)
  (let ((inhibit-message t))
    (use-package company-statistics :config (company-statistics-mode 1)))
  (use-package company-quickhelp
    :disabled true
    :config
    (setq company-quickhelp-delay 1)
    (company-quickhelp-mode 1))
  (global-company-mode 1))

(use-package projectile
  :bind (("C-c C-p p" . helm-projectile-switch-project))
  :init
  (setq projectile-use-git-grep t
        projectile-switch-project-action 'helm-projectile
        projectile-indexing-method 'hybrid
        projectile-enable-caching nil)
  :config
  (use-package helm-projectile
    :init (use-package helm)
    :config (helm-projectile-toggle 1))
  (define-key global-map (kbd "C-c C-p") 'projectile-command-map)
  (define-key global-map (kbd "C-c C-p C-s") 'projectile-save-project-buffers)
  ;; (define-key global-map (kbd "C-c g") 'helm-projectile-grep)
  (define-key global-map (kbd "C-c g") 'projectile-ag)
  (define-key projectile-mode-map (kbd "C-c g") 'projectile-ag)
  (define-key global-map (kbd "C-c TAB") 'helm-projectile-switch-to-buffer)
  (dolist (s '(".log" ".ai" ".svg" ".xml" ".zip" ".png" ".jpg"))
    (add-to-list 'grep-find-ignored-files (concat "*" s))
    (add-to-list 'helm-ag-ignore-patterns (concat "*" s))
    (add-to-list 'projectile-globally-ignored-file-suffixes s))
  (projectile-mode))

(use-package smex
  :bind ("M-x" . smex)
  :config (smex-initialize))

(use-package flycheck
  :defer t
  :init
  (setq flycheck-global-modes '(clojure-mode clojurescript-mode))
  (setq flycheck-disabled-checkers '(clojure-cider-typed))
  :config
  (progn (define-key flycheck-mode-map flycheck-keymap-prefix nil)
         (setq flycheck-keymap-prefix (kbd "C-c f"))
         (define-key flycheck-mode-map flycheck-keymap-prefix
           flycheck-command-map))
  (when nil
    (define-key flycheck-mode-map "\C-c ." 'flycheck-next-error)
    (define-key flycheck-mode-map "\C-c ," 'flycheck-previous-error))
  ;; because git-gutter is in the left fringe
  (setq flycheck-indication-mode 'right-fringe)
  ;; A non-descript, left-pointing arrow
  (use-package fringe-helper)
  (fringe-helper-define
    'flycheck-fringe-bitmap-double-arrow 'center
    "...X...."
    "..XX...."
    ".XXX...."
    "XXXX...."
    ".XXX...."
    "..XX...."
    "...X....")
  (use-package flycheck-pos-tip
    :config (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  '(global-flycheck-mode 1))

(use-package flx-ido
  :init
  (require 'ido)
  (setq ido-enable-flex-matching t
        ido-use-faces nil)
  (ido-mode 1)
  (ido-everywhere 1)
  :config
  (flx-ido-mode 1))

(use-package mic-paren
  :config (paren-activate))
;; (paren-deactivate)

(use-package paren-face
  :defer t
  :init
  (setq paren-face-regexp "[\\(\\)]")
  :config
  (global-paren-face-mode)
  (let ((color (cond ((symbol-matches custom-emacs-theme "zenburn")
                      "#808080")
                     (t "#707070"))))
    (face-spec-set 'parenthesis `((t (:foreground ,color)))))
  (defface square-brackets
    '((t (:foreground "#bbbf40")))
    'paren-face)
  (defface curly-brackets
    '((t (:foreground "#4f8f3d")))
    'paren-face)
  (defconst clojure-brackets-keywords
    '(("\\[" 0 'square-brackets)
      ("\\]" 0 'square-brackets)
      ("[\\{\\}]" 0 'curly-brackets)))
  (defun --custom-paren-face-mode-hook ()
    (if paren-face-mode
        (font-lock-add-keywords nil clojure-brackets-keywords t)
      (font-lock-remove-keywords nil clojure-brackets-keywords))
    (when (called-interactively-p 'any)
      (font-lock-fontify-buffer)))
  (add-hook 'paren-face-mode-hook '--custom-paren-face-mode-hook))

(use-package paredit
  :defer t
  :diminish (paredit-mode "()")
  :config
  (define-globalized-minor-mode real-global-paredit-mode
    paredit-mode (lambda ()
                   (if (not (minibufferp (current-buffer)))
                       (enable-paredit-mode))))
  (define-key paredit-mode-map (kbd "C-<left>") nil)
  (define-key paredit-mode-map (kbd "C-<right>") nil)
  (define-key paredit-mode-map (kbd "C-M-f") nil))

(use-package smartparens
  :config
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  (when nil
    (global-set-key (kbd "C-{")
                    (lambda (&optional arg)
                      (interactive "P")
                      (sp-wrap-with-pair "(")))
    (global-set-key (kbd "C-(")
                    (lambda (&optional arg)
                      (interactive "P")
                      (sp-wrap-with-pair "["))))
  (smartparens-global-mode t))

(defun do-git-gutter-config ()
  (define-key global-map "\C-xpp" 'git-gutter:popup-hunk)
  (define-key global-map "\C-xpr" 'git-gutter:revert-hunk)
  (define-key global-map "\C-xpn" 'git-gutter:next-hunk)
  (define-key global-map "\C-xpb" 'git-gutter:previous-hunk)
  (global-git-gutter-mode t))

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :if window-system
  :config (do-git-gutter-config))

(use-package git-gutter
  :diminish git-gutter-mode
  :if (null window-system)
  :config (do-git-gutter-config))

(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-x C-g" . magit-dispatch-popup)
  :config
  (define-key magit-status-mode-map (kbd "C-<tab>") nil)
  (diminish 'auto-revert-mode))

(defcustom jeffwk/enable-auto-neotree nil
  "Non-nil enables hooks to integrate neotree into various actions.")

(when jeffwk/enable-auto-neotree (load-local "neotree"))

;;;
;;; modes
;;;

(use-package systemd
  :mode
  ("\\.service\\'" . systemd-mode)
  ("\\.target\\'" . systemd-mode))

(use-package groovy-mode
  :mode "/Jenkinsfile"
  :config
  (setq groovy-indent-offset 2)
  (defun do-groovy-mode-config ()
    (setq-local tab-width 2))
  (add-hook 'groovy-mode-hook #'do-groovy-mode-config))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode
  ("README\\.md\\'" . gfm-mode)
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode)
  :init (setq markdown-command "multimarkdown")
  :config (use-package gh-md))

(use-package nginx-mode
  :mode "/nginx.conf$" "\\.nginx-site\\'"
  :config
  (unless use-global-aggressive-indent
    (add-hook 'nginx-mode-hook #'aggressive-indent-mode)))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :commands org-agenda org-store-link org-capture
  :config
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)
  (setq org-log-done t)
  (setq org-agenda-files (list "~/org/work.org" "~/org/self.org")))

(use-package pkgbuild-mode :mode "/PKGBUILD$")

;;;
;;; theming
;;;

(defvar override-faces nil)

(defun set-override-face (face spec)
  (face-spec-set face spec)
  (add-to-list 'override-faces face))
(defun set-override-faces (&rest face-specs)
  (dolist (fs face-specs)
    (destructuring-bind (face spec) fs
      (set-override-face face spec))))
(defun reset-override-faces ()
  (dolist (face override-faces)
    (face-spec-set face nil 'reset))
  (setq override-faces nil))

(use-package autothemer
  :disabled true)

(defun switch-to-theme (theme)
  ;; try to load elpa package for theme
  (cl-flet ((theme-p (s) (symbol-matches theme s)))
    (cond ((theme-p "sanityinc-tomorrow")
           (use-package color-theme-sanityinc-tomorrow))
          ((theme-p "sanityinc-solarized")
           (use-package color-theme-sanityinc-solarized))
          ((theme-p "gruvbox")          (use-package gruvbox-theme
                                          :ensure nil
                                          :load-path "~/.emacs.d/gruvbox-theme"
                                          :init
                                          (setq gruvbox-contrast 'hard)))
          ((theme-p "spacemacs-dark")   (use-package spacemacs-theme))
          ((theme-p "material")         (use-package material-theme))
          ((theme-p "ample")            (use-package ample-theme))
          ((theme-p "base16")           (use-package base16-theme))
          ((theme-p "zenburn")          (use-package zenburn-theme))
          ((theme-p "flatland")         (use-package flatland-theme))
          ((theme-p "moe")              (use-package moe-theme))
          ((theme-p "apropospriate")    (use-package apropospriate-theme))
          ((theme-p "molokai")          (use-package molokai-theme))
          ((theme-p "monokai")          (use-package monokai-theme)))
    ;; disable any current themes
    (dolist (active-theme custom-enabled-themes)
      (disable-theme active-theme))
    ;; activate theme
    (cond ((eql theme 'moe-dark)   (moe-dark))
          ((eql theme 'moe-light)  (moe-light))
          (t                       (load-theme theme t)))
    ;; reset any modified face specs
    (reset-override-faces)
    (cond ((theme-p "zenburn")
           (set-override-faces
            `(vertical-border
              ((t (:foreground "#7b7b6b"))))
            `(mode-line
              ((t (:font "Inconsolata Nerd Font Mono 24"
                         :foreground "#8fb28f"
                         :background "#2b2b2b"
                         :box ,(if nil nil `(:line-width -1 :color "#7a7a74"))))))
            `(mode-line-inactive
              ((t (:font "Inconsolata Nerd Font Mono 24"
                         :foreground "#5f7f5f"
                         :background "#383838"
                         :box ,(if nil nil `(:line-width -1 :color "#5b5b54"))))))))
          ((theme-p "gruvbox")
           (set-override-faces
            `(fringe ((t (:foreground "#373230" :background "#373230"))))
            `(line-number
              ((t (:font "Inconsolata Nerd Font 18"
                         :foreground "#7c6f64"
                         :background "#3c3836"))))
            `(line-number-current-line
              ((t (:font "Inconsolata Nerd Font 18"
                         :foreground "#fe8019"
                         :background "#3c3836"))))
            `(mode-line
              ((t (:font "Inconsolata Nerd Font Mono 24"
                         :foreground "#d5c4a1"
                         :background "#665c54"))))
            `(mode-line-inactive
              ((t (:font "Inconsolata Nerd Font Mono 24"
                         :foreground "#a89984"
                         :background "#3c3836")))))))))

(defun switch-custom-theme (&optional frame)
  (let ((frame (or (and (framep frame) frame)
                   (selected-frame))))
    (with-selected-frame frame
      (switch-to-theme custom-emacs-theme))))

(when (null window-system)
  ;; need to call this for terminal mode because the .Xdefaults settings won't apply
  (menu-bar-mode -1))

;;;
;;; languages
;;;

(defvar --use-lispy nil)

(defun ensure-lispy ()
  (when --use-lispy (use-package lispy)))

(use-package lispy
  :defer t
  :if --use-lispy
  :config 
  (defun enable-lispy (mode-hook)
    (add-hook mode-hook (lambda () (lispy-mode 1)))))

(use-package clojure-mode
  :mode
  ("\\.clj\\'" . clojure-mode)
  ("\\.cljs\\'" . clojurescript-mode)
  :config
  (use-package paredit)
  (ensure-lispy)
  (use-package paren-face)
  (use-package aggressive-indent)
  (use-package cider
    :diminish cider-mode
    :init
    (setq clojure-use-backtracking-indent t
          cider-repl-use-pretty-printing t
          cider-repl-popup-stacktraces t
          cider-auto-select-error-buffer t
          cider-prompt-for-symbol nil
          nrepl-use-ssh-fallback-for-remote-hosts t
          ;; cider-default-cljs-repl 'figwheel
          ;; cider-lein-command "/usr/local/bin/lein"
          )
    (ensure-tramp)
    :config
    (unless (exclude-pkg? 'auto-complete)
      (use-package ac-cider)
      (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
      (add-hook 'cider-mode-hook 'ac-cider-setup)
      (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
      (eval-after-load "auto-complete"
        '(progn
           (add-to-list 'ac-modes 'cider-mode)
           (add-to-list 'ac-modes 'cider-repl-mode))))
    (add-hook 'clojure-mode-hook #'cider-mode)
    (add-hook 'clojurescript-mode-hook #'cider-mode)
    (add-hook 'clojure-mode-hook 'turn-off-smartparens-mode)
    (add-hook 'clojurescript-mode-hook 'turn-off-smartparens-mode)
    (add-hook 'cider-repl-mode-hook 'turn-off-smartparens-mode)
    (add-hook 'clojure-mode-hook 'enable-paredit-mode)
    (add-hook 'clojurescript-mode-hook 'enable-paredit-mode)
    (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
    (defun my-cider-reload-repl-ns ()
      (cider-nrepl-request:eval
       (format "(require '%s :reload)"
               (buffer-local-value 'cider-buffer-ns (first (cider-repl-buffers))))
       (lambda (_response) nil)))
    (defvar cider-figwheel-connecting nil)
    (defun cider-figwheel-init ()
      (when cider-figwheel-connecting
        (pop-to-buffer (first (cider-repl-buffers)))
        (insert "(require 'figwheel-sidecar.repl-api)")
        (cider-repl-return)
        (insert "(figwheel-sidecar.repl-api/cljs-repl)")
        (cider-repl-return)
        (when (and nil (not (zerop (length cider-figwheel-connecting))))
          (insert (format "(require '%s)" cider-figwheel-connecting))
          (cider-repl-return)
          (insert (format "(in-ns '%s)" cider-figwheel-connecting))
          (cider-repl-return))))
    (add-hook 'nrepl-connected-hook 'cider-figwheel-init t)
    (defun cider-connect-figwheel (&optional port)
      (interactive)
      (let ((port (or port 7888))
            (cider-figwheel-connecting
             (if (member major-mode '(clojure-mode clojurescript-mode))
                 (clojure-expected-ns)
               "")))
        (cider-connect `(:host "localhost" :port ,port))))

    (define-key cider-mode-map (kbd "C-c C-p") nil)
    (define-key cider-repl-mode-map (kbd "C-c C-p") nil)
    (defun cider-load-buffer-reload-repl (&optional buffer)
      (interactive)
      (let ((result (if buffer
                        (cider-load-buffer buffer)
                      (cider-load-buffer))))
        (my-cider-reload-repl-ns)
        result))
    (define-key cider-mode-map (kbd "C-c C-k") 'cider-load-buffer-reload-repl)
    (defun my-cider-repl-set-ns (ns)
      (interactive (list (if (or (derived-mode-p 'cider-repl-mode)
                                 (null (cider-ns-form)))
                             (completing-read "Switch to namespace: "
                                              (cider-sync-request:ns-list))
                           (cider-current-ns))))
      (when-let ((buffer (first (cider-repl-buffers (cider-repl-type-for-buffer)))))
        (pop-to-buffer buffer)
        (insert (format "(require '%s)" ns))
        (cider-repl-return)
        (cider-repl-set-ns ns)))
    ;; (define-key cider-mode-map (kbd "C-c n") 'my-cider-repl-set-ns)
    (define-key cider-mode-map (kbd "C-c n") 'cider-repl-set-ns)
    (when --use-lispy
      (enable-lispy 'clojure-mode-hook)
      (enable-lispy 'cider-mode-hook)
      (enable-lispy 'cider-repl-mode-hook)))
  (use-package clj-refactor
    :diminish clj-refactor-mode
    :config
    (setq cljr-warn-on-eval nil
          cljr-suppress-middleware-warnings t)
    (defun clj-refactor-clojure-mode-hook ()
      (clj-refactor-mode 1)
      (yas-minor-mode 1)    ; for adding require/use/import statements
      ;; This choice of keybinding leaves cider-macroexpand-1 unbound
      (cljr-add-keybindings-with-prefix "C-c C-m"))
    (add-hook 'clojure-mode-hook #'clj-refactor-clojure-mode-hook)
    (add-hook 'clojurescript-mode-hook #'clj-refactor-clojure-mode-hook))
  (use-package flycheck-clojure
    :disabled true
    :config (flycheck-clojure-setup)))

(use-package slime
  :commands slime
  :mode
  ("\\.lisp\\'" . lisp-mode)
  ("\\.asd\\'" . lisp-mode)
  :init
  (ensure-tramp)
  (setq slime-contribs '(slime-fancy slime-tramp))
  :config
  (use-package paredit)
  (ensure-lispy)
  (use-package paren-face)
  (use-package aggressive-indent)
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

  (use-package projectile)
  (add-to-list 'projectile-globally-ignored-modes "comint-mode")
  (add-to-list 'projectile-globally-ignored-modes "slime-repl-mode")

  (unless use-global-aggressive-indent
    (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
    (add-hook 'slime-mode-hook #'aggressive-indent-mode))

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
  ;;(use-package slime-company)
  (use-package ac-slime
    :config
    (defun set-up-slime-ac-fuzzy () (set-up-slime-ac t))
    (add-hook 'slime-mode-hook 'set-up-slime-ac-fuzzy)
    (add-hook 'slime-repl-mode-hook 'set-up-slime-ac-fuzzy)))

(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (use-package paren-face)
   (require 'ielm)
   (use-package elisp-slime-nav :diminish "")
   (turn-on-elisp-slime-nav-mode)
   (use-package paredit)
   (ensure-lispy)
   (unless use-global-aggressive-indent
     (use-package aggressive-indent)
     (aggressive-indent-mode))
   (turn-off-smartparens-mode)
   (enable-paredit-mode)
   (when --use-lispy (lispy-mode 1))
   (eldoc-mode 1)))

(use-package scala-mode
  :mode
  ("\\.scala\\'" . scala-mode)
  ("\\.sbt\\'" . scala-mode)
  :config
  (use-package ensime
    :diminish ensime-mode
    :config
    (setq ensime-startup-snapshot-notification nil)
    (setq ensime-auto-generate-config t)
    (setq ensime-typecheck-idle-interval 0.3)
    (setq ensime-completion-style 'company)
    (use-package company)
    (when nil
      (add-hook 'scala-mode-hook (lambda () (auto-complete-mode -1))))
    (when nil
      (add-hook 'scala-mode-hook
                (lambda ()
                  (add-hook 'post-command-hook
                            (lambda ()
                              (when (and ensime-mode (ensime-connected-p))
                                (ensime-print-errors-at-point)))
                            t t))))
    (define-key scala-mode-map "\C-t" 'ensime-type-at-point)
    (define-key scala-mode-map "\C-\M-e" 'ensime-print-errors-at-point)
    (define-key scala-mode-map "\C-c." 'ensime-forward-note)
    (define-key scala-mode-map "\C-c," 'ensime-backward-note)
    (define-key scala-mode-map (kbd "C-M-.") 'ensime-show-uses-of-symbol-at-point)))

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

;;;
;;; web dev
;;;

(use-package less-css-mode
  :mode "\\.less\\'" "\\.variables\\'" "\\.overrides\\'")

(use-package web-mode
  :mode "\\.js\\'" "\\.jsx\\'" "\\.json\\'"
  :config
  ;; (use-package tern)
  (use-package flycheck)
  (use-package js2-mode
    :config
    (flycheck-add-mode 'javascript-eslint 'js2-mode)
    (flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)
    (setq js2-include-node-externs t)
    (setq js2-include-browser-externs t)
    (setq js2-strict-trailing-comma-warning nil)
    (setq js2-indent-switch-body t)
    (defun my-js2-mode-hook ()
      (setq js2-basic-offset 2)
      (flycheck-mode 1)
      ;; (tern-mode t)
      (when (executable-find "eslint")
        (flycheck-select-checker 'javascript-eslint)))
    (add-hook 'js2-mode-hook 'my-js2-mode-hook)
    (add-hook 'js2-jsx-mode-hook 'my-js2-mode-hook))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
  (add-to-list 'flycheck-disabled-checkers 'json-jsonlist)
  (defun my-web-mode-hook ()
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (flycheck-mode 1)
    ;; (tern-mode t)
    (when (executable-find "eslint")
      (flycheck-select-checker 'javascript-eslint)))
  (add-hook 'web-mode-hook 'my-web-mode-hook))

(use-package jade-mode :mode "\\.jade\\'")

;;;
;;; Set up copy/paste and daemon
;;;

;; This sets up terminal-mode Emacs instances to use the X shared clipboard
;; for kill and yank commands.
;;
;; Emacs needs to be started after the X server for this to work.
;; My solution is to run a script (/usr/local/bin/emacs-reload)
;; in my i3wm config file to restart the emacs daemons upon
;; logging into an X session.
(unless (mac?)
  (defun xsel-paste ()
    (shell-command-to-string "xsel -ob"))
  (defun xsel-copy (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "xsel -ib" "*Messages*" "xsel" "-ib")))
        (process-send-string proc text)
        (process-send-eof proc))))
  (defun do-xsel-copy-paste-setup ()
    (when (and (null window-system)
               (getenv "DISPLAY")
               (file-exists-p "/usr/bin/xsel")
               (not (equal (user-login-name) "root")))
      (setq interprogram-cut-function 'xsel-copy)
      (setq interprogram-paste-function 'xsel-paste)))
  (do-xsel-copy-paste-setup))

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
(setq server-socket-dir (format "/tmp/%s/emacs%d" (user-login-name) (user-uid)))

(use-package powerline
  :config
  (setq powerline-height 40
        ;; powerline-default-separator nil
        powerline-default-separator 'arrow
        powerline-display-buffer-size nil
        powerline-display-mule-info nil
        powerline-display-hud nil
        powerline-gui-use-vcs-glyph t
        ;; powerline-text-scale-factor nil
        )
  (powerline-default-theme))

(use-package spaceline
  :disabled true
  :init
  (setq powerline-height 40
        powerline-default-separator 'arrow
        ;; spaceline-inflation 1.4
        spaceline-separator-dir-left '(right . right)
        spaceline-separator-dir-right '(right . right)
        spaceline-workspace-numbers-unicode t)
  :config
  (require 'spaceline-config)
  ;; (spaceline-compile)

  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (spaceline-toggle-buffer-position-on)
  (spaceline-toggle-hud-off)
  (spaceline-toggle-line-column-on)

  (defface jeffwk/modeline-buffer-path
    '((t
       (:inherit mode-line-buffer-id
                 :bold nil
                 :foreground "#a89984")))
    "Face used for the buffer name."
    :group '+jeffwk)

  (defun jeffwk/buffer-path ()
    (when (and buffer-file-name
               (projectile-project-p)
               (projectile-project-root))
      (let ((buffer-path
             (file-relative-name (file-name-directory
                                  (or buffer-file-truename (file-truename buffer-file-name)))
                                 (projectile-project-root))))
        (unless (equal buffer-path "./")
          (let ((max-length (truncate (* (window-body-width) 0.4))))
            (if (> (length buffer-path) max-length)
                (let* ((path (nreverse (split-string buffer-path "/" t)))
                       ;; (path (subseq path 0 (min (length path) 2)))
                       (output ""))
                  (when (and path (equal "" (car path)))
                    (setq path (cdr path)))
                  (while (and path (<= (length output) (- max-length 4)))
                    (setq output (concat (car path) "/" output)
                          path (cdr path)))
                  (when path
                    (setq output (concat "../" output)))
                  (unless (string-suffix-p "/" output)
                    (setq output (concat output "/")))
                  output)
              buffer-path))))))

  (spaceline-define-segment
   buffer-id-with-path
   "Name of buffer (or path relative to project root)."
   (let ((name (propertize (if (buffer-file-name)
                               (file-name-nondirectory (buffer-file-name))
                             (buffer-name))
                           'face 'mode-line-buffer-id))
         (path (jeffwk/buffer-path)))
     (if path
         (concat (propertize path 'face
                             '(:inherit jeffwk/modeline-buffer-path))
                 name)
       name)))

  (defun jeffwk/spaceline-theme ()
    (spaceline-install
     `((((((persp-name :fallback workspace-number)
           window-number) :separator "|")
         buffer-modified
         buffer-size)
        :face highlight-face
        :priority 0)
       (anzu :priority 4)
       auto-compile
       ((buffer-id-with-path remote-host)
        :priority 5)
       major-mode
       (process :when active)
       ((flycheck-error flycheck-warning flycheck-info)
        :when active
        :priority 3)
       (minor-modes :when active)
       (mu4e-alert-segment :when active)
       (erc-track :when active)
       (version-control :when active
                        :priority 7)
       (org-pomodoro :when active)
       (org-clock :when active)
       nyan-cat)
     `(which-function
       (python-pyvenv :fallback python-pyenv)
       purpose
       (battery :when active)
       (selection-info :priority 2)
       input-method
       ((buffer-encoding-abbrev
         point-position
         line-column)
        :separator " | "
        :priority 3)
       (global :when active)
       (buffer-position :priority 0)
       (hud :priority 0)))

    (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

  ;; (spaceline-spacemacs-theme)
  ;; (spaceline-emacs-theme)
  (jeffwk/spaceline-theme))

(use-package all-the-icons :if jeffwk/enable-auto-neotree)
;;(all-the-icons-install-fonts)

(use-package doom-themes
  :config
  (add-hook 'after-init-hook #'doom-themes-visual-bell-config)
  (add-hook 'after-init-hook #'doom-themes-neotree-config)
  (setq doom-neotree-enable-variable-pitch t
        doom-neotree-file-icons 'simple
        doom-neotree-line-spacing 2)
  (doom-themes-visual-bell-config)
  (when jeffwk/enable-auto-neotree
    (doom-themes-neotree-config)))

(defun jeffwk/init-ui (&optional frame)
  (switch-custom-theme)
  (scroll-bar-mode -1)
  ;;(menu-bar-mode -1)
  (tool-bar-mode -1)
  (when (gui-mac?)
    ;;(set-frame-font "Source Code Pro 19")
    ;;(set-frame-font "SauceCodePro Nerd Font Medium 19")
    ;;(set-frame-font "Sauce Code Powerline 19")
    ;;(set-frame-font "Fira Code Retina-13")
    ;;(set-frame-font "Inconsolata for Powerline 20")
    (set-frame-font "Inconsolata Nerd Font Mono 26"))

  (cond ((equal system-name "jeff-osx")
         (set-frame-width nil 100)
         (set-frame-height nil 48))
        ((equal system-name "jeff-mbp")
         nil))
  ;;(when (graphical?) (global-display-line-numbers-mode 1))
  nil)

(jeffwk/init-ui)
(add-hook 'after-make-frame-functions #'jeffwk/init-ui)

(load-local "auto-margin")

(setq file-name-handler-alist file-name-handler-alist-backup
      inhibit-message nil)

(when (graphical?)
  ;; (add-hook 'after-init-hook 'helm-projectile-switch-project)
  (when jeffwk/enable-auto-neotree (use-package neotree)))

(defun all-sesman-sessions ()
  (sesman-sessions (sesman--system) t))

(defun test-buffer-name (buf regexp &optional exclude-regexp)
  (and (string-match regexp (buffer-name buf))
       (if (null exclude-regexp) t
         (not (string-match exclude-regexp (buffer-name buf))))))

(defun match-buffer-name (regexp &optional exclude-regexp)
  (remove-if-not (lambda (buf)
                   (test-buffer-name buf regexp exclude-regexp))
                 (buffer-list)))

(defun match-sesman-session (regexp &optional exclude-regexp)
  (first
   (cl-remove-if (lambda (ses)
                   (not (test-buffer-name (second ses) regexp exclude-regexp)))
                 (all-sesman-sessions))))

(defun stop-cider-all ()
  (interactive)
  (dolist (buf (match-buffer-name "\*cider-repl\ .*"))
    (save-excursion
      (switch-to-buffer buf)
      (cider-quit))))

(defun run-cider-project (project-name
                          project-file-path
                          clj-file-path
                          cljs-file-path
                          clj-test-file-path
                          figwheel-port
                          cljs-user-ns)
  (lexical-let ((project-name project-name)
                (project-file-path project-file-path)
                (clj-file-path clj-file-path)
                (cljs-file-path cljs-file-path)
                (clj-test-file-path clj-test-file-path)
                (figwheel-port figwheel-port)
                (cljs-user-ns cljs-user-ns))
    (cl-labels
        ((open-project
          ()
          (find-file project-file-path))
         (start-clj
          ()
          (save-excursion
            (find-file clj-file-path)
            (cider-connect
             `(:host
               "localhost"
               :port
               ,(second (assoc project-name (cider-locate-running-nrepl-ports)))))))
         (start-cljs
          ()
          (save-excursion
            (find-file cljs-file-path)
            (cider-connect-figwheel))
          (when cljs-user-ns
            (run-with-timer
             0.5 nil
             (lambda ()
               (let ((cljs-repl (first (match-buffer-name
                                        (format ".*cider-repl.*%s.*%d.*"
                                                project-name figwheel-port)))))
                 (when cljs-repl
                   (save-excursion
                     (switch-to-buffer cljs-repl)
                     (insert (format " (in-ns '%s)" cljs-user-ns)))))))))
         (link-sesman-dirs
          ()
          (save-excursion
            (find-file clj-file-path)
            (when-let ((clj-ses (match-sesman-session
                                 (format ".*cider-repl.*%s.*" project-name)
                                 (format "%d" figwheel-port))))
              (sesman-link-with-directory nil clj-ses)))
          (save-excursion
            (find-file cljs-file-path)
            (when-let ((cljs-ses (match-sesman-session
                                  (format ".*cider-repl.*%s.*%d.*"
                                          project-name figwheel-port))))
              (sesman-link-with-directory nil cljs-ses)))
          (save-excursion
            (find-file clj-test-file-path)
            (when-let ((clj-test-ses (match-sesman-session
                                      (format ".*cider-repl.*%s.*" project-name)
                                      (format "%d" figwheel-port))))
              (sesman-link-with-directory nil clj-test-ses))
            (kill-buffer)))
         (show-repl-buffers
          ()
          (let ((clj-repl (first (match-buffer-name
                                  (format ".*cider-repl.*%s.*" project-name)
                                  (format "%d" figwheel-port))))
                (cljs-repl (first (match-buffer-name
                                   (format ".*cider-repl.*%s.*%d.*"
                                           project-name figwheel-port)))))
            (delete-other-windows)
            (when clj-repl
              (switch-to-buffer clj-repl))
            (when cljs-repl
              (if clj-repl
                  (switch-to-buffer-other-window cljs-repl)
                (switch-to-buffer cljs-repl))))))
      (stop-cider-all)
      (open-project)
      (start-clj)
      (start-cljs)
      (link-sesman-dirs)
      (show-repl-buffers))))

(defun sysrev ()
  (interactive)
  (run-cider-project
   "sysrev"
   "~/code/sysrev/project.clj"
   "~/code/sysrev/src/clj/sysrev/user.clj"
   "~/code/sysrev/src/cljs/sysrev/user.cljs"
   "~/code/sysrev/test/clj/sysrev/test/core.clj"
   7888
   "sysrev.user"))
