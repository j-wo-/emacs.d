;;; -*- lexical-binding: t -*-

(set-language-environment "utf-8")

(require 'cl-lib)

(defun graphical? () (cl-some #'display-graphic-p (frame-list)))
(defun laptop? () (or (equal (system-name) "jeff-mbp")
                      (equal (system-name) "jeff-laptop")))
(defun mac? () (eql system-type 'darwin))
(defun gui-mac-std? () (eql window-system 'ns))
(defun gui-emacs-mac? () (eql window-system 'mac))
(defun gui-mac? () (or (gui-mac-std?) (gui-emacs-mac?)))
(defun wayland? () (and (getenv "DISPLAY") (getenv "WAYLAND_DISPLAY") t))

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
        "~/bin/vcsh/launch-sway-programs"
        "~/bin/vcsh/makepkg-chroot"
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
    (magit-status-setup-buffer)
    (sysrev)))

;;; Favorite themes (dark)
;; 'base16-eighties
;; 'leuven-dark
;; 'alect-dark
;; 'apropospriate-dark
;; 'zenburn
;; 'gruvbox-dark-soft 'gruvbox-dark-medium 'gruvbox-dark-hard
;; 'sanityinc-tomorrow-night
;; 'sanityinc-tomorrow-eighties
;; 'monokai

;;; Favorite themes (light)
;; 'sanityinc-solarized-light
;; 'leuven
;; 'anti-zenburn

;;; Terminal themes (null background)
;; 'sanityinc-tomorrow-night-rxvt
;; 'gruvbox-dark-hard

;;; More themes
;; 'moe-dark
;; 'alect-black
;; 'sanityinc-solarized-dark
;; 'flatland
;; 'spacemacs-dark
;; 'material
;; 'molokai
;; 'base16-default-dark
;; 'cyberpunk

(defvar custom-emacs-theme
  ;; (if (graphical?) 'base16-eighties 'gruvbox-dark-medium)
  ;; (if (graphical?) 'zenburn 'gruvbox-dark-medium)
  ;; (if (graphical?) 'gruvbox-dark-soft 'gruvbox-dark-medium)
  (if (graphical?) 'gruvbox-dark-medium 'gruvbox-dark-medium)
  ;; (if (graphical?) 'sanityinc-solarized-light 'gruvbox-dark-medium)
  ;; (if (graphical?) 'sanityinc-solarized-dark 'gruvbox-dark-medium)
  ;; (if (graphical?) 'sanityinc-tomorrow-night 'gruvbox-dark-medium)
  ;; (if (graphical?) 'leuven 'gruvbox-dark-medium)
  ;; (if (graphical?) 'gruvbox-dark-medium 'gruvbox-dark-medium)
  )

;; (defvar custom-emacs-theme 'sanityinc-tomorrow-night)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; (setq-default fill-column 70)

(global-auto-revert-mode t)
(transient-mark-mode t)

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

;;;
;;; Package setup
;;;

(require 'package)

(unless (and nil (>= emacs-major-version 27))
  (package-initialize))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(defun pin-pkg (pkg archive)
  (add-to-list 'package-pinned-packages (cons pkg archive) t))

(defun unpin-pkg (pkg)
  (setq package-pinned-packages
        (cl-remove-if (lambda (x) (eql (cl-first x) pkg))
                      package-pinned-packages)))

;;;
;;; use-package bootstrapping (support automatic bootstrap from empty elpa library)
;;;

(pin-pkg 'use-package "melpa")

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

(require 'use-package)
(setq use-package-always-ensure t)

;; This is to get around an infinite recursion in emacs-lisp-mode-hook
;; when bootstrapping packages.
(ensure-installed paren-face elisp-slime-nav paredit)

(defun cleanup-buffer ()
  (interactive)
  (when (not indent-tabs-mode)
    (untabify (point-min) (point-max)))
  (delete-trailing-whitespace (point-min) (point-max)))

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

(defun print-to-buffer (x)
  ;;(insert "\n" (prin1-to-string x))
  (princ (concat "\n" (with-output-to-string (print x))) (current-buffer)))

(defun active-minor-modes ()
  (--filter (and (boundp it) (symbol-value it)) minor-mode-list))
;;(print-to-buffer (active-minor-modes))

(defun minor-mode-active-p (minor-mode)
  (if (member minor-mode (active-minor-modes)) t nil))

(defun load-local (file &optional no-compile)
  (let ((path (locate-user-emacs-file file))
        ;;(ext ".el")
        (ext (if no-compile ".el" ".elc"))
        (no-byte-compile no-compile))
    (unless no-compile
      (byte-recompile-file (concat path ".el") nil 0 nil))
    (load (concat path ext) nil t t)))

(load-local "keys")
(load-local "commands")

(windmove-default-keybindings '(shift))
(windmove-default-keybindings '(control meta))

;;;
;;; load packages
;;;

(defvar jeff/exclude-pkgs
  '(evil auto-complete --company))

(defun exclude-pkg? (pkg)
  (member pkg jeff/exclude-pkgs))

(use-package dash)

(use-package diminish
  :config
  (diminish 'eldoc-mode)
  (diminish 'isearch-mode))

(use-package vterm
  :commands vterm)

(use-package outshine
  :defer t
  :pin melpa
  :diminish outshine-mode
  :init (require 'outline)
  :config
  (diminish 'outline-minor-mode
            (if (graphical?) " " " ▼")) ;; " " " " " "
  (add-hook 'outline-minor-mode-hook 'outshine-mode)
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (defvar outline-display-table (make-display-table))
  (set-display-table-slot outline-display-table 'selective-display
                          ;; (vector (make-glyph-code ?▼ 'escape-glyph))
                          (vector " "))
  (defun set-outline-display-table ()
    (setf buffer-display-table outline-display-table))
  (add-hook 'outline-mode-hook 'set-outline-display-table)
  (add-hook 'outline-minor-mode-hook 'set-outline-display-table))

(use-package outline
  :defer t
  :config
  (defun -use-package-outshine ()
    (use-package outshine))
  (add-hook 'outline-minor-mode-hook '-use-package-outshine))

(use-package evil
  :disabled true
  :pin melpa
  :config
  (evil-mode 1)
  (diminish 'undo-tree-mode)
  (use-package evil-lisp-state
    :pin melpa
    :config
    (evil-lisp-state-leader ", l")))

;;;
;;; general
;;;

(use-package disable-mouse
  :if (laptop?)
  :config
  (diminish 'global-disable-mouse-mode)
  (diminish 'disable-mouse-mode)
  (diminish 'disable-mouse-global-mode)
  (disable-mouse-global-mode))

(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh")
  (add-to-list 'tramp-methods '("vcsh"
                                (tramp-login-program "vcsh")
                                (tramp-login-args
                                 (("enter")
                                  ("%h")))
                                (tramp-remote-shell "/bin/sh")
                                (tramp-remote-shell-args
                                 ("-c")))))

(defun symbol-suffix (sym suffix)
  (intern (concat (symbol-name sym) suffix)))

(defmacro set-mode-name (mode name)
  (let ((func-name (intern (concat "--set-mode-name--" (symbol-name mode)))))
    `(progn
       (defun ,func-name () (setq mode-name ,name))
       (add-hook ',(symbol-suffix mode "-hook") #',func-name 100)
       (when (eql major-mode ',mode)
         (,func-name)))))

(set-mode-name emacs-lisp-mode "ELisp")

(defun rainbow-mode-1 () (rainbow-mode 1))

(defun use-rainbow-mode (mode &optional mode-hook)
  (use-package rainbow-mode)
  (let ((mode-hook (or mode-hook (symbol-suffix mode "-hook"))))
    (when (symbolp mode-hook)
      (add-hook mode-hook 'rainbow-mode-1))))

(use-package rainbow-mode
  :defer 0.5
  :diminish rainbow-mode
  :init
  (setq rainbow-html-colors t
        rainbow-html-colors-alist nil
        rainbow-ansi-colors 'auto
        rainbow-x-colors nil
        rainbow-latex-colors nil
        rainbow-r-colors nil)
  :config
  (require 'shell)
  (require 'sh-script)
  (dolist (mode '(sh-mode shell-mode))
    (use-rainbow-mode mode)))

(use-package sh-script
  :mode (("/zshrc$"     . sh-mode)
         ("/zshenv$"    . sh-mode)
         ("/zprofile$"  . sh-mode)
         ("/zlogin$"    . sh-mode)
         ("/zlogout$"   . sh-mode)
         ("/zpreztorc$" . sh-mode)
         ("\\.zsh\\'"   . sh-mode)
         ("/prompt_.*_setup$" . sh-mode))
  :init (setq sh-basic-offset 2)
  :config
  (set-mode-name sh-mode "Sh")
  (defun jeff/sh-mode-hook () (setq-local tab-width 2))
  (add-hook 'sh-mode-hook 'jeff/sh-mode-hook))

(use-package python-mode
  :pin melpa
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq py-company-pycomplete-p nil)
  (use-rainbow-mode 'python-mode)
  (set-mode-name python-mode "Python"))

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
  ;; helm-map helm-ag-map helm-do-ag-map
  (use-package helm-ag
    :init (setq helm-ag-insert-at-point 'symbol
                helm-ag-use-temp-buffer t
                helm-ag-use-grep-ignore-list t))
  '(setq helm-grep-input-idle-delay 0.7
         helm-input-idle-delay 0.035))

(use-package esup
  :disabled t
  :commands esup)

(use-package yasnippet
  :pin melpa
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
    (cl-remove-if-not 'car frames)))

(defun function-stack ()
  "Like call-stack but is a list of only the function names"
  (butlast (mapcar 'cl-second (call-stack))))

(defvar jeff/use-global-aggressive-indent t)

(defvar jeff/indent-disable-functions nil)
(defvar jeff/indent-disable-cider nil)

(when jeff/indent-disable-cider
  (setq jeff/indent-disable-functions
        (list 'company-capf 'all-completions 'cider-complete
              'nrepl-send-sync-request 'cider-sync-request:complete
              'accept-process-output)))

(defun jeff/indent-disable-function-active ()
  (unless (null jeff/indent-disable-functions)
    (let ((active-fnames (function-stack)))
      (cl-some (lambda (fname) (member fname active-fnames))
               jeff/indent-disable-functions))))

(defun jeff/load-aggressive-indent ()
  (add-to-list 'load-path "~/.emacs.d/aggressive-indent-mode")
  (require 'aggressive-indent)
  ;; " "
  ;; " "
  ;; unicode 2004 U+2004 (thick space)
  (diminish 'aggressive-indent-mode (if (graphical?) " " " Aggr"))
  (setq aggressive-indent-sit-for-time 0.025)
  (unless (null jeff/indent-disable-functions)
    (add-to-list 'aggressive-indent-dont-indent-if
                 'jeff/indent-disable-function-active))
  (when jeff/use-global-aggressive-indent
    (dolist (mode '(cider-repl-mode))
      (add-to-list 'aggressive-indent-excluded-modes mode))
    (aggressive-indent-global-mode)))

(jeff/load-aggressive-indent)

(use-package aggressive-indent
  ;; :pin "melpa-stable"
  :disabled t
  :ensure nil
  :load-path "~/.emacs.d/aggressive-indent-mode"
  :init (jeff/load-aggressive-indent))

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
  :pin melpa
  :defer 0.25
  :if (not (exclude-pkg? 'company))
  :diminish company-mode global-company-mode
  :init
  ;; http://emacs.stackexchange.com/a/10838/12585
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-tooltip-offset-display 'scrollbar ; 'lines
        company-minimum-prefix-length 3
        company-idle-delay 0.15)
  :config
  (add-to-list 'company-transformers 'company-sort-by-occurrence)
  (let ((inhibit-message t))
    (use-package company-statistics :config (company-statistics-mode 1)))
  (use-package company-quickhelp
    :config
    (setq company-quickhelp-delay 0.5)
    (company-quickhelp-mode 1))
  (global-company-mode 1))

(defcustom jeff/use-projectile-ag nil
  "Use ag for projectile search if non-nil; otherwise use git grep."
  :group 'jeff)

(defvar jeff/projectile-search-fn
  (if jeff/use-projectile-ag 'helm-projectile-ag 'helm-projectile-grep))

(use-package projectile
  :init
  (setq projectile-use-git-grep t
        projectile-switch-project-action 'helm-projectile
        projectile-indexing-method 'hybrid
        projectile-enable-caching t
        projectile-mode-line-prefix "")
  :bind (("C-c p"       . projectile-command-map)
         ("C-c C-p"     . projectile-command-map)
         ("C-c TAB"     . helm-projectile-switch-to-buffer)
         ("C-c g"       . helm-projectile-grep)
         ("C-c G"       . projectile-grep)
         ("C-c C-s"     . helm-projectile-ag)
         ("C-c s"       . helm-projectile-ag)
         ("C-c S"       . projectile-ag))
  :config
  (use-package helm-projectile
    :init (use-package helm)
    :config (helm-projectile-toggle 1))
  (define-map-keys projectile-command-map
    ("p"            'helm-projectile-switch-project)
    ("S"            'projectile-save-project-buffers)
    ("C-s"          'projectile-save-project-buffers))
  (define-map-keys-multi (global-map projectile-mode-map)
    ("C-c p"        'projectile-command-map)
    ("C-c C-p"      'projectile-command-map)
    ("C-c TAB"      'helm-projectile-switch-to-buffer)
    ("C-c g"        'helm-projectile-grep)
    ("C-c G"        'projectile-grep)
    ("C-c C-s"      'helm-projectile-ag)
    ("C-c s"        'helm-projectile-ag)
    ("C-c S"        'projectile-ag))
  (dolist (s '(".log" ".ai" ".svg" ".xml" ".zip" ".png" ".jpg"))
    (add-to-list 'grep-find-ignored-files (concat "*" s))
    ;; (add-to-list 'helm-ag-ignore-patterns (concat "*" s))
    (add-to-list 'helm-grep-ignored-files (concat "*" s))
    (add-to-list 'projectile-globally-ignored-file-suffixes s))
  (projectile-mode))

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :init
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  :config
  (smex-initialize))

(defcustom jeff/flycheck-global t
  "Runs (global-flycheck-mode 1) if non-nil."
  :group 'jeff)

;;(print-to-buffer byte-compile-warning-types)

;; (redefine
;;  callargs
;;  free-vars
;;  unresolved
;;  obsolete
;;  noruntime
;;  cl-functions
;;  interactive-only
;;  make-local
;;  mapcar
;;  constants
;;  suspicious
;;  lexical)

(use-package flycheck
  :pin melpa
  :defer 0.25
  :init
  (setq flycheck-global-modes '(not org-mode)
        ;; '(clojure-mode clojurec-mode clojurescript-mode groovy-mode)
        flycheck-disabled-checkers '(clojure-cider-typed emacs-lisp-checkdoc)
        ;; because git-gutter is in the left fringe
        flycheck-indication-mode 'right-fringe)
  :config
  ;; redefine flycheck-keymap-prefix
  (progn
    (define-key flycheck-mode-map flycheck-keymap-prefix nil)
    (setq flycheck-keymap-prefix (kbd "C-c f"))
    (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map))
  (when t
    (define-map-keys flycheck-mode-map
      ("C-c ." 'flycheck-next-error)
      ("C-c ," 'flycheck-previous-error)))
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
    :config (setq flycheck-display-errors-function 'flycheck-pos-tip-error-messages))
  (when jeff/flycheck-global
    (global-flycheck-mode 1)))

(use-package flx-ido
  :defer 0.25
  :init
  (require 'ido)
  (setq ido-enable-flex-matching t
        ido-use-faces nil)
  (ido-mode 1)
  (ido-everywhere 1)
  :config
  (flx-ido-mode 1))

(use-package mic-paren
  :defer 0.5
  :config (paren-activate))
;; (paren-deactivate)
;; (--with-elapsed-time-alert (paren-activate) (paren-deactivate) (paren-activate))

(defun theme? (theme)
  (let ((theme (if (symbolp theme) (symbol-name theme) theme)))
    (symbol-matches custom-emacs-theme theme)))

(use-package paren-face
  :defer 0.5
  :config
  (setq paren-face-regexp "[\\(\\)]")
  (global-paren-face-mode)
  (let ((paren-color  (cond ((theme? 'zenburn) "#808080")
                            ((theme? 'alect-dark) "#848484")
                            (t "#707070")))
        (square-color (cond ((theme? 'zenburn) "#bfc438")
                            ((theme? 'alect-dark) "#c4c830")
                            (t "#bbbf40")))
        (curly-color  (cond ((theme? 'zenburn) "#66a818")
                            ((theme? 'alect-dark) "#6bb018")
                            (t "#4f8f3d"))))
    (face-spec-set 'parenthesis `((t (:foreground ,paren-color))))
    (defface square-brackets
      `((t (:foreground ,square-color)))
      'paren-face)
    (defface curly-brackets
      `((t (:foreground ,curly-color)))
      'paren-face))
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
  :diminish paredit-mode
  :config
  (define-globalized-minor-mode real-global-paredit-mode
    paredit-mode (lambda ()
                   (if (not (minibufferp (current-buffer)))
                       (enable-paredit-mode))))
  (define-map-keys paredit-mode-map
    ("C-<left>"     nil)
    ("C-<right>"    nil)
    ("C-M-<left>"   nil)
    ("C-M-<right>"  nil)
    ("C-M-<up>"     nil)
    ("C-M-<down>"   nil)
    ("C-M-f"        nil)))

(use-package smartparens
  :defer 0.25
  :config
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  (diminish 'smartparens-mode " SP")
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

(defun activate-paxedit ()
  (use-package paxedit)
  (paxedit-mode 1))

(use-package paxedit
  :defer t
  :diminish "Pax"
  :pin melpa
  :config
  (setq paxedit-alignment-cleanup t
        paxedit-whitespace-cleanup t)
  (define-map-keys paxedit-mode-map
    ("s-<right>"      'paxedit-transpose-forward)
    ("s-<left>"       'paxedit-transpose-backward)
    ("s-<up>"         'paxedit-backward-up)
    ("s-<down>"       'paxedit-backward-end)
    ("s-b"            'paxedit-previous-symbol)
    ("s-f"            'paxedit-next-symbol)
    ("s-c"            'paxedit-copy)
    ("s-k"            'paxedit-kill)
    ("s-<backspace>"  'paxedit-delete)
    ("M-s-<up>"       'paxedit-sexp-raise)
    ;; symbol backward/forward kill
    ("C-w"            'paxedit-backward-kill)
    ("M-w"            'paxedit-forward-kill)
    ;; symbol manipulation
    ("M-u"            'paxedit-symbol-change-case)
    ("M-s-k"          'paxedit-symbol-kill)
    ("M-s-c"          'paxedit-symbol-copy)
    ;; special
    ("s-d"            'paxedit-dissolve)
    ("s-0"            'paxedit-compress)
    ("s-1"            'paxedit-format-1)
    ;; parens
    ("("              'paxedit-open-round)
    ("["              'paxedit-open-bracket)
    ("{"              'paxedit-open-curly)
    ("s-'"            'paxedit-open-quoted-round))
  (add-to-list 'emacs-lisp-mode-hook 'activate-paxedit))

(defun do-git-gutter-config ()
  (define-map-keys global-map
    ("C-x p p" 'git-gutter:popup-hunk)
    ("C-x p r" 'git-gutter:revert-hunk)
    ("C-x p n" 'git-gutter:next-hunk)
    ("C-x p b" 'git-gutter:previous-hunk))
  (global-git-gutter-mode t))

(use-package git-gutter-fringe
  :defer 0.2
  :diminish git-gutter-mode
  :if window-system
  :config (do-git-gutter-config))

(use-package git-gutter
  :defer 0.2
  :diminish git-gutter-mode
  :if (null window-system)
  :config (do-git-gutter-config))

(use-package magit
  :pin melpa
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-dispatch-popup))
  :config
  (diminish 'auto-revert-mode)
  (define-map-keys magit-status-mode-map
    ("C-<tab>" nil))
  (use-package projectile))

(use-package paradox
  :defer t
  :pin melpa
  :commands list-packages paradox-list-packages
  :config (let ((inhibit-message t))
            (paradox-enable)))

(defcustom jeff/enable-auto-neotree nil
  "Non-nil enables hooks to integrate neotree into various actions."
  :group 'jeff)

(when jeff/enable-auto-neotree
  (load-local "neotree"))

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
  :pin melpa
  :commands markdown-mode gfm-mode
  :mode
  ("README\\.md\\'" . gfm-mode)
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode)
  :init (setq markdown-command "multimarkdown")
  :config (use-package gh-md))

(use-package nginx-mode
  :mode "/nginx.conf$" "\\.nginx-site\\'"
  :config
  (unless jeff/use-global-aggressive-indent
    (add-hook 'nginx-mode-hook #'aggressive-indent-mode)))

(use-package alert
  :pin melpa
  :config
  (setq alert-default-style 'libnotify
        alert-fade-time 5))

(defun jeff/init-time (&optional as-string)
  (let ((init-time (float-time
                    (time-subtract after-init-time before-init-time))))
    (if as-string (format "%.2fs" init-time) init-time)))
;;(jeff/init-time t)

(defun jeff/load-org-notify ()
  (interactive)
  (when (not (featurep 'org-notify))
    (use-package org)
    (add-to-list 'load-path "~/.emacs.d/org-notify")
    (require 'org-notify)
    (setq org-notify-interval 600)))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (setq org-log-done 'time
        org-agenda-files '("~/org/self.org"
                           "~/org/schedule.org"
                           "~/org/work.org"
                           ;; "~/org/sysrev.org"
                           "~/code/sysrev/sysrev-tasks.org")
        org-agenda-timegrid-use-ampm t)
  (define-map-keys org-mode-map
    ("C-S-<left>"     'org-metaleft)
    ("C-S-<right>"    'org-metaright)
    ("C-S-<down>"     'org-metadown)
    ("C-S-<up>"       'org-metaup)
    ("C-S-<return>"   'org-meta-return))
  ;; unbind conflicting keys
  (define-map-keys org-mode-map
    ("C-c C-p"        nil)
    ("C-c p"          nil)
    ("C-<tab>"        nil)
    ("M-s a"          'org-agenda-list)
    ("M-s s"          'org-schedule)
    ("M-s d"          'org-deadline)
    ("M-s p"          'org-pomodoro)
    ("M-s c"          'org-alert-check))
  (use-package org-ql
    :pin melpa)
  (use-package org-present
    :pin melpa)
  (use-package org-projectile
    :pin melpa)
  (use-package helm-org-rifle
    :pin melpa)
  (use-package org-super-agenda
    :pin melpa)
  (use-package org-gcal
    :pin melpa)
  (use-package org-fancy-priorities
    :pin melpa)
  (jeff/load-org-notify)
  (use-package org-alert
    :disabled t
    :load-path "~/.emacs.d/org-alert"
    :config
    (setq org-alert-interval 600
          org-alert-fade-time 10)
    (org-alert-disable))
  (use-package org-bullets
    :pin melpa
    :config
    (add-hook 'org-mode-hook 'org-bullets-mode)
    ;; ◉ ○ ✸ ✿ ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
    ;; ► • ★ ▸
    (setq org-bullets-bullet-list '("●" "◉")
          ;; org-bullets-bullet-list '("◆" "◇")
          ))
  (use-package org-pomodoro
    ;; https://github.com/marcinkoziej/org-pomodoro
    ;; M-x org-pomodoro (activate on org task)
    :config
    (defun jeff/org-clock-heading ()
      "")
    (setq org-pomodoro-length 25
          org-pomodoro-short-break-length 5
          org-pomodoro-audio-player (executable-find "mpv-quiet")
          org-clock-clocked-in-display nil
          ;; org-clock-string-limit 1
          org-clock-heading-function #'jeff/org-clock-heading))
  (use-package mpv
    :config
    (org-add-link-type "mpv" #'mpv-play)
    (add-hook 'org-open-at-point-functions #'mpv-seek-to-position-at-point)
    (defun org-mpv-complete-link (&optional arg)
      (replace-regexp-in-string "file:" "mpv:"
                                (org-file-complete-link arg)
                                t t))))

(use-package pkgbuild-mode :mode "/PKGBUILD")

(use-package yaml-mode :mode "\\.yml\\'")

;;;
;;; theming
;;;

(defvar override-faces nil)

(defun set-override-face (face spec)
  (face-spec-set face spec)
  (add-to-list 'override-faces face))

(defun set-override-faces (&rest face-specs)
  (dolist (fs face-specs)
    (cl-destructuring-bind (face spec) fs
      (set-override-face face spec))))

(defun reset-override-faces ()
  (dolist (face override-faces)
    (face-spec-set face nil 'reset))
  (setq override-faces nil))

(defcustom modeline-font "InputMono Nerd Font:medium:pixelsize=25"
  ;; "InputMono Nerd Font:pixelsize=23"
  ;; "AnkaCoder Nerd Font:pixelsize=24"
  ;; "Inconsolata Nerd Font 13"
  ;; "Inconsolata Nerd Font:pixelsize=24"
  "Alternate font used for modeline."
  :group 'jeff)

(defun switch-to-theme (theme)
  (cl-flet ((theme-p (s) (symbol-matches theme s)))
    (let ((lnum-font "Inconsolata:pixelsize=22")
          (lnum-weight1 'semi-bold)
          (lnum-weight2 'semi-bold))
      ;; load elpa package for theme
      (cond ((theme-p "sanityinc-tomorrow")
             (use-package color-theme-sanityinc-tomorrow))
            ((theme-p "sanityinc-solarized")
             (use-package color-theme-sanityinc-solarized))
            ((theme-p "gruvbox")          (progn
                                            (use-package autothemer)
                                            (use-package gruvbox-theme
                                              :ensure nil
                                              :load-path "~/.emacs.d/gruvbox-theme")))
            ;; ((theme-p "spacemacs-dark")   (use-package spacemacs-theme))
            ((theme-p "material")         (use-package material-theme))
            ((theme-p "ample")            (use-package ample-theme))
            ((theme-p "base16")           (use-package base16-theme))
            ((theme-p "zenburn")          (use-package zenburn-theme))
            ((theme-p "anti-zenburn")     (use-package anti-zenburn-theme))
            ((theme-p "flatland")         (use-package flatland-theme))
            ((theme-p "moe")              (use-package moe-theme))
            ((theme-p "apropospriate")    (use-package apropospriate-theme))
            ((theme-p "molokai")          (use-package molokai-theme))
            ((theme-p "monokai")          (use-package monokai-theme))
            ((theme-p "leuven")           (use-package leuven-theme))
            ((theme-p "cyberpunk")        (use-package cyberpunk-theme))
            ((theme-p "alect")            (use-package alect-themes
                                            :ensure nil
                                            :load-path "~/.emacs.d/alect-themes")))
      ;; disable any current themes
      (dolist (active-theme custom-enabled-themes)
        (disable-theme active-theme))
      ;; reset any modified face specs
      (reset-override-faces)
      (set-override-faces `(line-number
                            ((t (:font ,lnum-font :weight ,lnum-weight1))))
                          `(line-number-current-line
                            ((t (:font ,lnum-font :weight ,lnum-weight2)))))
      (cond ((theme-p "alect-dark")
             (alect-create-theme dark))
            ((theme-p "alect-dark-alt")
             (alect-create-theme dark t))
            ((theme-p "alect-black")
             (alect-create-theme black))
            ((theme-p "alect-light")
             (alect-create-theme light)))
      ;; activate theme
      (cond ((eql theme 'moe-dark)   (moe-dark))
            ((eql theme 'moe-light)  (moe-light))
            (t                       (load-theme theme t)))
      ;; set face specs based on theme
      (cond ((theme-p "zenburn")
             (set-override-faces
              `(vertical-border
                ((t (:foreground "#7b7b6b"))))
              `(mode-line
                ((t (:font ,modeline-font
                           :box nil
                           ;; :foreground "#8fb28f"
                           ;; :background "#2b2b2b"
                           ))))
              `(mode-line-inactive
                ((t (:font ,modeline-font
                           ;; :foreground "#5f7f5f"
                           ;; :background "#383838"
                           :box nil))))))
            ((theme-p "gruvbox")
             (set-override-faces
              ;; `(fringe ((t (:background "#373230"))))
              ;; `(fringe ((t (:background "#332c2a"))))
              `(fringe ((t (:background "#37312b"))))
              `(line-number
                ((t (;; :foreground "#7c6f64" :background "#3c3836"
                     ;; :foreground "#6c5f54" :background "#363230"
                     ;; :foreground "#5f5046" :background "#37312b"
                     :foreground "#635448" :background "#37312b"
                     :font ,lnum-font :weight ,lnum-weight1))))
              `(line-number-current-line
                ((t (:foreground "#fe8019" :background "#37312b"
                                 :font ,lnum-font :weight ,lnum-weight2))))
              `(mode-line
                ((t (:font ,modeline-font :foreground "#d5c4a1" :background "#665c54"))))
              `(mode-line-inactive
                ((t (:font ,modeline-font :foreground "#a89984" :background "#3c3836"))))))
            ((theme-p "alect")
             (set-override-faces))
            ((theme-p "eighties")
             (set-override-faces
              `(fringe ((t (:background "#404040"))))
              `(vertical-border ((t (:foreground "#505050"))))
              `(mode-line
                ((t (:font ,modeline-font
                           :box (:line-width -2 :color "#555555" :style nil)))))
              `(mode-line-inactive
                ((t (:font ,modeline-font
                           :box (:line-width -2 :color "#555555" :style nil)))))))
            (t
             (set-override-faces
              `(fringe ((t (:background "#2a292c"))))
              ;; `(fringe ((t (:background "#373230"))))
              `(vertical-border ((t (:foreground "#505050"))))
              `(mode-line
                ((t (:font ,modeline-font
                           :box (:line-width -1 :color "#555555" :style nil)
                           ;; :box nil
                           ))))
              `(mode-line-inactive
                ((t (:font ,modeline-font
                           :box (:line-width -1 :color "#555555" :style nil)
                           ;; :box nil
                           )))))))
      ;; ensure powerline colors are updated
      (powerline-reset)
      (powerline-default-theme))))

(defun switch-custom-theme (&optional frame)
  (let ((frame (or (and (framep frame) frame)
                   (selected-frame))))
    (with-selected-frame frame
      (switch-to-theme custom-emacs-theme))))

(require 'faces)

;; modified from faces.el to rewrite #xxxxxx00 to #xxxxxx
(defun color-values (color &optional frame)
  "Return a description of the color named COLOR on frame FRAME.
COLOR should be a string naming a color (e.g. \"white\"), or a
string specifying a color's RGB components (e.g. \"#ff12ec\").

Return a list of three integers, (RED GREEN BLUE), each between 0
and either 65280 or 65535 (the maximum depends on the system).
Use `color-name-to-rgb' if you want RGB floating-point values
normalized to 1.0.

If FRAME is omitted or nil, use the selected frame.
If FRAME cannot display COLOR, the value is nil.

COLOR can also be the symbol `unspecified' or one of the strings
\"unspecified-fg\" or \"unspecified-bg\", in which case the
return value is nil."
  (when (and (stringp color)
             (string-equal (substring color 0 1) "#")
             (= (length color) (+ 1 8)))
    (setq color (substring color 0 7)))
  (cond
   ((member color '(unspecified "unspecified-fg" "unspecified-bg"))
    nil)
   ((memq (framep (or frame (selected-frame))) '(x w32 ns))
    (xw-color-values color frame))
   (t
    (tty-color-values color frame))))

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

(use-package cider
  :defer t
  :pin melpa
  :diminish cider-mode
  :init
  (setq clojure-use-backtracking-indent t
        clojure-indent-style 'always-align ;; 'align-arguments
        cider-repl-use-pretty-printing t
        cider-auto-select-error-buffer t
        cider-prompt-for-symbol nil
        cider-save-file-on-load t
        nrepl-use-ssh-fallback-for-remote-hosts t
        cider-preferred-build-tool 'shadow-cljs
        cider-default-cljs-repl 'shadow-select
        cider-shadow-default-options ":dev")
  (use-package tramp)
  :config
  (let ((inhibit-message t))
    ;; (setq-default clojure-docstring-fill-column 70)
    (unless (exclude-pkg? 'auto-complete)
      (use-package ac-cider)
      (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
      (add-hook 'cider-mode-hook 'ac-cider-setup)
      (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
      (use-package auto-complete)
      (add-to-list 'ac-modes 'cider-mode)
      (add-to-list 'ac-modes 'cider-repl-mode))
    (set-mode-name clojure-mode "CLJ")
    (set-mode-name clojurescript-mode "CLJS")
    (set-mode-name clojurec-mode "CLJC")
    (add-hook 'clojure-mode-hook 'cider-mode)
    (add-hook 'clojurescript-mode-hook 'cider-mode)
    (add-hook 'clojure-mode-hook 'turn-off-smartparens-mode)
    (add-hook 'clojurescript-mode-hook 'turn-off-smartparens-mode)
    (add-hook 'cider-repl-mode-hook 'turn-off-smartparens-mode)
    (add-hook 'clojure-mode-hook 'enable-paredit-mode)
    (add-hook 'clojurescript-mode-hook 'enable-paredit-mode)
    (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
    (add-hook 'clojure-mode-hook 'paxedit-mode)
    (add-hook 'clojurescript-mode-hook 'paxedit-mode)
    (defun my-cider-reload-repl-ns ()
      (let ((ns (buffer-local-value 'cider-buffer-ns (cl-first (cider-repl-buffers)))))
        (when ns
          (cider-nrepl-request:eval
           (format "(require '%s :reload)" ns)
           (lambda (_response) nil)))))
    (defvar cider-figwheel-connecting nil)
    (defun cider-figwheel-init ()
      (when cider-figwheel-connecting
        (pop-to-buffer (cl-first (cider-repl-buffers)))
        (insert "(require 'figwheel-sidecar.repl-api)")
        (cider-repl-return)
        (insert "(figwheel-sidecar.repl-api/cljs-repl)")
        (cider-repl-return)
        (when (and nil (not (zerop (length cider-figwheel-connecting))))
          (insert (format "(require '%s)" cider-figwheel-connecting))
          (cider-repl-return)
          (insert (format "(in-ns '%s)" cider-figwheel-connecting))
          (cider-repl-return))))
    ;; (add-hook 'nrepl-connected-hook 'cider-figwheel-init t)
    (defun cider-connect-figwheel (&optional port)
      (interactive)
      (let ((port (or port 7888))
            (cider-figwheel-connecting
             (if (member major-mode '(clojure-mode clojurescript-mode))
                 (clojure-expected-ns)
               "")))
        (cider-connect `(:host "localhost" :port ,port))))
    (defun cider-load-buffer-reload-repl (&optional buffer)
      (interactive)
      (let ((result (if buffer
                        (cider-load-buffer buffer)
                      (cider-load-buffer))))
        (my-cider-reload-repl-ns)
        result))
    (add-hook 'cider-file-loaded-hook 'my-cider-reload-repl-ns)
    (when --use-lispy
      (enable-lispy 'clojure-mode-hook)
      (enable-lispy 'cider-mode-hook)
      (enable-lispy 'cider-repl-mode-hook))
    (define-map-keys cider-mode-map
      ("C-c C-k" 'cider-load-buffer)
      ("C-c n" 'cider-repl-set-ns)
      ("C-c C-p" nil))
    (define-map-keys cider-repl-mode-map
      ("C-c C-p" nil))))

(use-package clj-refactor
  :defer t
  :pin melpa
  :diminish clj-refactor-mode
  :config
  (setq cljr-warn-on-eval nil
        cljr-suppress-middleware-warnings t)
  (defun clj-refactor-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1)  ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (add-hook 'clojure-mode-hook #'clj-refactor-clojure-mode-hook)
  (add-hook 'clojurescript-mode-hook #'clj-refactor-clojure-mode-hook))

(use-package flycheck-clojure
  :defer t
  :pin melpa
  ;; :disabled t
  :init (use-package flycheck)
  :config (flycheck-clojure-setup))

(use-package flycheck-clj-kondo
  :defer t
  :init (use-package flycheck)
  :config (require 'flycheck-clj-kondo))

(use-package clojure-mode
  :pin melpa
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.edn\\'" . clojure-mode))
  :config
  (let ((inhibit-message t))
    (use-package paredit)
    (ensure-lispy)
    (use-package paren-face)
    (use-package cider)
    (use-package clj-refactor)
    (use-package flycheck-clojure)
    (use-package flycheck-clj-kondo)))

(use-package slime
  :disabled t
  :pin melpa
  :commands slime
  :mode
  ("\\.lisp\\'" . lisp-mode)
  ("\\.asd\\'" . lisp-mode)
  :init
  (use-package tramp)
  (setq slime-contribs '(slime-fancy slime-tramp))
  :config
  (use-package paredit)
  (ensure-lispy)
  (use-package paren-face)
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
  (define-map-keys slime-mode-map
    ;; ("<return>" 'paredit-newline)
    ("C-c ." 'slime-next-note)
    ("C-c ," 'slime-previous-note))
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

  (unless jeff/use-global-aggressive-indent
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

  ;;(use-package slime-annot)
  ;;(use-package slime-company)
  (use-package ac-slime
    :config
    (defun set-up-slime-ac-fuzzy () (set-up-slime-ac t))
    (add-hook 'slime-mode-hook 'set-up-slime-ac-fuzzy)
    (add-hook 'slime-repl-mode-hook 'set-up-slime-ac-fuzzy)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (use-package paxedit)
            (use-package rainbow-mode)
            (use-package paren-face)
            (require 'ielm)
            (use-package elisp-slime-nav :diminish elisp-slime-nav-mode)
            (turn-on-elisp-slime-nav-mode)
            (use-package paredit)
            (ensure-lispy)
            (turn-off-smartparens-mode)
            (enable-paredit-mode)
            (when --use-lispy (lispy-mode 1))
            (eldoc-mode 1)
            (rainbow-mode 1)))

(use-package scala-mode
  :mode
  ("\\.scala\\'" . scala-mode)
  ("\\.sbt\\'" . scala-mode)
  :config
  (use-package ensime
    :disabled t
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
    (define-map-keys scala-mode-map
      ("C-t"     'ensime-type-at-point)
      ("C-M-e"   'ensime-print-errors-at-point)
      ("C-c ."   'ensime-forward-note)
      ("C-c ,"   'ensime-backward-note)
      ("C-M-."   'ensime-show-uses-of-symbol-at-point))))

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
  :mode "\\.less\\'" "\\.variables\\'" "\\.overrides\\'"
  :config (use-rainbow-mode 'less-css-mode))

(use-package js2-mode
  :pin melpa
  :mode ("\\.js\\'" "\\.json\\'"
         "\\.config/waybar/config\\'")
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

(use-package web-mode
  :pin melpa
  ;; :mode ("\\.js\\'" "\\.jsx\\'" "\\.json\\'")
  :mode "\\.jsx\\'"
  :config
  (use-package tern
    :disabled t
    :pin melpa)
  (use-package flycheck)
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
;;
;; This sets up terminal-mode Emacs instances to use the X shared clipboard
;; for kill and yank commands.
;;
;; Emacs needs to be started after the X server for this to work.
;; My solution is to run a script (/usr/local/bin/emacs-reload)
;; in my i3wm config file to restart the emacs daemons upon
;; logging into an X session.
(defun xsel-paste ()
  (shell-command-to-string "xsel -ob"))
(defun xsel-copy (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "xsel -ib" "*Messages*" "xsel" "-ib")))
      (process-send-string proc text)
      (process-send-eof proc))))
(defun do-xsel-copy-paste-setup ()
  (when (and (not (mac?))
             (null window-system)
             (getenv "DISPLAY")
             (file-exists-p "/usr/bin/xsel")
             (not (equal (user-login-name) "root")))
    (setq interprogram-cut-function 'xsel-copy)
    (setq interprogram-paste-function 'xsel-paste)))
;;;
;;; copy/paste for Wayland
;;;
(defun wl-paste ()
  (shell-command-to-string "wl-paste -n"))
(defun wl-copy (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "wl-copy" "*Messages*" "wl-copy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(defun do-wayland-copy-paste-setup ()
  (when (and (wayland?)
             (null window-system)
             (file-exists-p "/usr/bin/wl-copy")
             (file-exists-p "/usr/bin/wl-paste")
             (not (equal (user-login-name) "root")))
    (setq interprogram-cut-function 'wl-copy)
    (setq interprogram-paste-function 'wl-paste)))

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

(defcustom jeff/use-spaceline nil
  "Uses spaceline for modeline if non-nil."
  :group 'jeff)

(use-package powerline
  :if (not jeff/use-spaceline)
  :config
  (setq powerline-height 29
        ;; powerline-height 48
        powerline-default-separator 'arrow
        powerline-display-buffer-size nil
        powerline-display-mule-info nil
        powerline-display-hud nil
        powerline-gui-use-vcs-glyph t
        powerline-text-scale-factor 0.85)
  (powerline-reset)
  (powerline-default-theme)
  (force-mode-line-update))

(when jeff/use-spaceline
  (load-local "spaceline" t))

(use-package all-the-icons :if jeff/enable-auto-neotree)
;; (all-the-icons-install-fonts)

(use-package doom-themes
  :config
  (add-hook 'after-init-hook #'doom-themes-visual-bell-config)
  ;;(add-hook 'after-init-hook #'doom-themes-neotree-config)
  (setq doom-neotree-enable-variable-pitch t
        doom-neotree-file-icons 'simple
        doom-neotree-line-spacing 2)
  (doom-themes-visual-bell-config)
  (when jeff/enable-auto-neotree
    (doom-themes-neotree-config)))

(use-package default-text-scale
  :defer 0.5
  :config
  (setq default-text-scale-amount 10)
  (default-text-scale-mode)
  (define-map-keys global-map
    ("C-x =" 'default-text-scale-increase)
    ("C-x -" 'default-text-scale-decrease)
    ("C-x 0" 'default-text-scale-reset)))

(load-local "auto-cider")

(defun sysrev ()
  (interactive)
  (run-cider-project
   "sysrev"
   "~/code/sysrev/project.clj"
   "~/code/sysrev/src/clj/sysrev/user.clj"
   "~/code/sysrev/src/cljs/sysrev/user.cljs"
   "~/code/sysrev/test/clj/sysrev/test/core.clj"
   7888
   "sysrev.user"
   '("(->> (all-projects) count time)")
   '("@(subscribe [:active-panel])")))

(defun --cider-quit-all ()
  (interactive)
  (use-package projectile)
  (stop-cider-all)
  (save-excursion
    (find-file "~/code/sysrev/project.clj")
    (dolist (b (projectile-project-buffers))
      (kill-buffer b))))

(defun --benchmark-sysrev ()
  (interactive)
  (use-package projectile)
  (--cider-quit-all)
  (with-delay 0.1 (garbage-collect))
  (with-delay 0.25 (sysrev))
  (with-delay 2.5 (--cider-quit-all)))

(defun jeff/init-copy-paste ()
  (cond ((mac?)     nil)
        ((wayland?) (do-wayland-copy-paste-setup))
        (t          (do-xsel-copy-paste-setup))))

(defun jeff/init-ui (&optional frame initial full)
  (when (or initial full)
    (switch-custom-theme))

  ;;(scroll-bar-mode -1)
  ;;(menu-bar-mode -1)
  ;;(tool-bar-mode -1)

  (when (and full (gui-mac?))
    ;;(set-frame-font "Source Code Pro 19")
    ;;(set-frame-font "SauceCodePro Nerd Font Medium 19")
    ;;(set-frame-font "Sauce Code Powerline 19")
    ;;(set-frame-font "Fira Code-13")
    ;;(set-frame-font "Inconsolata for Powerline 15")
    (set-frame-font "Inconsolata Nerd Font Mono 26" nil t))

  ;;(set-frame-font "Inconsolata for Powerline 16")
  ;;(set-frame-font "SauceCodePro Medium:pixelsize=26")
  ;;(set-frame-font "Inconsolata for Powerline:pixelsize=31")
  ;;(set-frame-font "Inconsolata Nerd Font Mono:pixelsize=29")
  ;;(set-frame-font "InconsolataGo Nerd Font Mono:pixelsize=29")
  ;;(set-frame-font "Anka/Coder Condensed:pixelsize=28")
  ;;(set-frame-font "AnkaCoder Nerd Font Mono:pixelsize=26")
  ;;(set-frame-font "Anka/Coder:pixelsize=26")
  ;;(set-frame-font "Input Mono Medium:pixelsize=25")
  ;;(set-frame-font "InputMono Nerd Font:pixelsize=27")
  ;;(set-frame-font "InputMono Nerd Font Medium 20")
  ''(when (and full (graphical?))
      (set-frame-font "InputMono Nerd Font:medium:pixelsize=26" nil t))

  (cond ((equal (system-name) "jeff-osx")
         (set-frame-width nil 100)
         (set-frame-height nil 48))
        ((equal (system-name) "jeff-mbp")
         nil))

  (when (and initial (graphical?))
    (global-display-line-numbers-mode 1))

  (when full
    (powerline-reset)
    (powerline-default-theme))

  nil)

(load-local "auto-margin")

(when (graphical?)
  ;; (add-hook 'after-init-hook 'helm-projectile-switch-project)
  (when jeff/enable-auto-neotree (use-package neotree)))

;; make sure all deferred packages are still installed on initial bootstrap
(ensure-installed outshine rainbow-mode python-mode helm yasnippet company projectile
                  helm-projectile smex flycheck fringe-helper flycheck-pos-tip mic-paren
                  paredit paxedit magit paradox systemd groovy-mode markdown-mode nginx-mode
                  org org-bullets org-pomodoro mpv pkgbuild-mode clojure-mode cider
                  clj-refactor flycheck-clojure flycheck-clj-kondo less-css-mode
                  web-mode js2-mode)

(defun native-comp? ()
  (and (functionp 'native-comp-available-p)
       (native-comp-available-p)))

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

(defun --elapsed-alert (title elapsed)
  (let ((alert-fade-time 3))
    (alert (format "Elapsed: %.3fs" elapsed)
           :title title)))

(defmacro --with-elapsed-time-alert (&rest body)
  `(--elapsed-alert (--body-title ',body)
                    (--with-elapsed-time ,@body)))

;;(--with-elapsed-time-alert (+ 1 2))

(defun jeff/native-comp-all ()
  (interactive)
  (jeff/native-comp-path "~/.emacs.d/elpa/")
  ;; (jeff/native-comp-path "/usr/share/emacs/28.0.50/")
  (jeff/native-comp-path "/usr/local/share/emacs/28.0.50/lisp/")
  t)

(defun jeff/native-comp-path (path)
  (when (native-comp?)
    (let ((comp-always-compile t))
      (native-compile-async path t nil))))

(defun jeff/describe-init ()
  (interactive)
  (let ((alert-fade-time (if (display-graphic-p) 5 2)))
    (alert (format "Emacs started in %s" (jeff/init-time t))
           :title (format "Emacs <%s>" (buffer-name))
           :category 'emacs-init)))

(defun jeff/after-init ()
  (jeff/init-ui nil t)
  (jeff/init-copy-paste)
  (force-window-update)
  (redraw-frame)
  (run-with-timer 1.0 nil 'jeff/describe-init)
  (garbage-collect))

(add-hook 'post-command-hook 'force-mode-line-update)

(add-hook 'after-init-hook 'jeff/after-init)
;;(add-hook 'after-make-frame-functions 'jeff/init-ui)

;;(byte-recompile-file "~/.emacs.d/init.el" nil 0 nil)

;; Local Variables:
;; byte-compile-warnings: (not free-vars make-local callargs)
;; End:
