(setq
 ;; startup time optimization
 ;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
 file-name-handler-alist-backup file-name-handler-alist
 file-name-handler-alist nil
 gc-cons-threshold-default gc-cons-threshold
 gc-cons-threshold (* 100 1000 1000)
 ;; prevent echoing messages while loading
 inhibit-message nil
 inhibit-splash-screen t)

(defun restore-config-post-init ()
  (setq inhibit-message nil
        file-name-handler-alist file-name-handler-alist-backup)
  (run-with-idle-timer
   1.0 nil
   (lambda ()
     (setq gc-cons-threshold (* 10 1000 1000)))))

(add-hook 'after-init-hook 'restore-config-post-init)

(require 'cl-lib)

(defun graphical? () (cl-some #'display-graphic-p (frame-list)))
(defun laptop? () (or (equal system-name "jeff-mbp")
                      (equal system-name "jeff-laptop")))
(defun mac? () (eql system-type 'darwin))
(defun gui-mac-std? () (eql window-system 'ns))
(defun gui-emacs-mac? () (eql window-system 'mac))
(defun gui-mac? () (or (gui-mac-std?) (gui-emacs-mac?)))

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
  (if (graphical?) 'base16-eighties 'gruvbox-dark-medium))

;; (defvar custom-emacs-theme 'sanityinc-tomorrow-night)

(set-language-environment "utf-8")

(setq default-frame-alist '((left-fringe . 18) (right-fringe . 18))
      custom-safe-themes t
      auto-save-default nil
      vc-follow-symlinks t
      make-backup-files nil
      echo-keystrokes 0.1)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; (setq-default fill-column 70)

(global-auto-revert-mode t)
(transient-mark-mode t)

(defun symbol-matches (sym str)
  (not (null (string-match-p str (symbol-name sym)))))

(defmacro define-map-keys (map &rest defs)
  `(progn
     ,@(mapcar (lambda (entry)
                 (cl-destructuring-bind (kbd-str func) entry
                   `(define-key ,map (kbd ,kbd-str) ,func)))
               defs)))

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
(ensure-installed paren-face elisp-slime-nav paredit aggressive-indent)

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
(add-hook 'write-file-hooks
          (lambda ()
            (cleanup-buffer)
            nil))

(defun active-minor-modes ()
  (--filter (and (boundp it) (symbol-value it)) minor-mode-list))
(defun minor-mode-active-p (minor-mode)
  (if (member minor-mode (active-minor-modes)) t nil))

(defun load-local (file &optional no-byte-compile)
  (let ((path (locate-user-emacs-file file))
        (ext (if no-byte-compile ".el" ".elc")))
    (unless no-byte-compile
      (byte-recompile-file (concat path ".el") nil 0 nil))
    (load (concat path ext) nil t t)))

(load-local "keys")
(load-local "commands")

(windmove-default-keybindings)

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

(use-package outshine
  :diminish outshine-mode
  :init (require 'outline)
  :config
  (diminish 'outline-minor-mode
            (if (graphical?) " " " ▼")) ;; " " " " " "
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
  :defer 0.25
  :config
  (diminish 'global-disable-mouse-mode)
  (diminish 'disable-mouse-mode)
  (diminish 'disable-mouse-global-mode)
  (disable-mouse-global-mode))

(defun --tramp-config ()
  (setq tramp-default-method "ssh")
  (add-to-list 'tramp-methods '("vcsh"
                                (tramp-login-program "vcsh")
                                (tramp-login-args
                                 (("enter")
                                  ("%h")))
                                (tramp-remote-shell "/bin/sh")
                                (tramp-remote-shell-args
                                 ("-c")))))

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

(ensure-tramp)

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
         ("\\.zsh\\'"   . sh-mode))
  :config
  (set-mode-name sh-mode "Sh"))

(use-package python-mode
  :pin melpa
  :mode ("\\.py\\'" . python-mode)
  :config
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
                helm-ag-use-temp-buffer t)))

(use-package esup
  :disabled true
  :commands esup)

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

(use-package aggressive-indent
  :config
  (diminish 'aggressive-indent-mode (if (graphical?) " " " Aggr")) ;; " "
  (setq aggressive-indent-sit-for-time 0.05)
  (unless (null jeff/indent-disable-functions)
    (add-to-list 'aggressive-indent-dont-indent-if
                 'jeff/indent-disable-function-active))
  (when jeff/use-global-aggressive-indent
    (dolist (mode '(cider-repl-mode))
      (add-to-list 'aggressive-indent-excluded-modes mode))
    (aggressive-indent-global-mode)))

(when jeff/use-global-aggressive-indent
  (use-package aggressive-indent))

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
  :pin melpa-stable
  :defer 0.5
  :if (not (exclude-pkg? 'company))
  :diminish company-mode global-company-mode
  :init
  ;; http://emacs.stackexchange.com/a/10838/12585
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-tooltip-offset-display 'lines ; 'scrollbar
        company-minimum-prefix-length 3
        company-idle-delay 0.15)
  :config
  (add-to-list 'company-transformers 'company-sort-by-occurrence)
  (let ((inhibit-message t))
    (use-package company-statistics :config (company-statistics-mode 1)))
  (use-package company-quickhelp
    :if t
    :config
    (setq company-quickhelp-delay 1)
    (company-quickhelp-mode 1))
  (global-company-mode 1))

(defcustom jeff/use-projectile-ag nil
  "Use ag for projectile search if non-nil; otherwise use git grep."
  :group 'jeff)

(use-package projectile
  :bind (("C-c C-p p" . helm-projectile-switch-project))
  :init
  (setq projectile-use-git-grep t
        projectile-switch-project-action 'helm-projectile
        projectile-indexing-method 'hybrid ; 'alien
        projectile-enable-caching t
        projectile-mode-line-prefix "")
  :config
  (use-package helm-projectile
    :init (use-package helm)
    :config
    (helm-projectile-toggle 1)
    (setq helm-ag-use-grep-ignore-list t)
    '(setq helm-grep-input-idle-delay 0.7
           helm-input-idle-delay 0.035))
  (let ((search-fn (if jeff/use-projectile-ag 'projectile-ag 'projectile-grep)))
    (define-map-keys projectile-mode-map
      ("C-c g" search-fn))
    (define-map-keys global-map
      ("C-c g" search-fn)
      ("C-c C-p" 'projectile-command-map)
      ("C-c C-p C-s" 'projectile-save-project-buffers)
      ("C-c TAB" 'projectile-switch-to-buffer)))
  (dolist (s '(".log" ".ai" ".svg" ".xml" ".zip" ".png" ".jpg"))
    (add-to-list 'grep-find-ignored-files (concat "*" s))
    (add-to-list 'helm-ag-ignore-patterns (concat "*" s))
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

(defcustom jeff/flycheck-global nil
  "Runs (global-flycheck-mode 1) if non-nil."
  :group 'jeff)

(use-package flycheck
  :defer t
  :init
  (setq flycheck-global-modes '(clojure-mode clojurescript-mode)
        flycheck-disabled-checkers '(clojure-cider-typed)
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

(when jeff/flycheck-global
  (use-package flycheck))

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
  :defer 0.5
  :config (paren-activate))
;; (paren-deactivate)

(defun theme? (theme)
  (let ((theme (if (symbolp theme) (symbol-name theme) theme)))
    (symbol-matches custom-emacs-theme theme)))

(use-package paren-face
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
    ("C-<left>" nil)
    ("C-<right>" nil)
    ("C-M-f" nil)))

(use-package smartparens
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
  :diminish git-gutter-mode
  :if window-system
  :config (do-git-gutter-config))

(use-package git-gutter
  :diminish git-gutter-mode
  :if (null window-system)
  :config (do-git-gutter-config))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-dispatch-popup))
  :config
  (diminish 'auto-revert-mode)
  (define-map-keys magit-status-mode-map
    ("C-<tab>" nil)))

(use-package paradox
  :pin melpa
  :commands list-packages paradox-list-packages
  :config (paradox-enable))

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
    ("C-S-<return>"   'org-meta-return)
    ("M-<return>"     'org-meta-return))
  ;; unbind conflicting keys
  (define-map-keys org-mode-map
    ("C-c C-p" nil)
    ("C-<tab>" nil))
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
    (cl-destructuring-bind (face spec) fs
      (set-override-face face spec))))

(defun reset-override-faces ()
  (dolist (face override-faces)
    (face-spec-set face nil 'reset))
  (setq override-faces nil))

(defcustom modeline-font "Inconsolata Nerd Font 13" ;; "Inconsolata Nerd Font:pixelsize=26"
  "Alternate font used for modeline."
  :group 'jeff)

(defun switch-to-theme (theme)
  (cl-flet ((theme-p (s) (symbol-matches theme s)))
    (let ()
      ;; load elpa package for theme
      (cond ((theme-p "sanityinc-tomorrow")
             (use-package color-theme-sanityinc-tomorrow))
            ((theme-p "sanityinc-solarized")
             (use-package color-theme-sanityinc-solarized))
            ((theme-p "gruvbox")          (progn
                                            (use-package autothemer)
                                            (use-package gruvbox-theme
                                              :ensure nil
                                              :load-path "~/.emacs.d/gruvbox-theme"
                                              :init
                                              (setq gruvbox-contrast 'hard))))
            ((theme-p "spacemacs-dark")   (use-package spacemacs-theme))
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
            ((theme-p "leuven")           (use-package leuven-theme :pin melpa))
            ((theme-p "cyberpunk")        (use-package cyberpunk-theme :pin melpa))
            ((theme-p "alect")            (use-package alect-themes
                                            :ensure nil
                                            :load-path "~/.emacs.d/alect-themes")))
      ;; disable any current themes
      (dolist (active-theme custom-enabled-themes)
        (disable-theme active-theme))
      ;; reset any modified face specs
      (reset-override-faces)
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
              `(fringe ((t (:background "#373230"))))
              `(line-number
                ((t (:foreground "#7c6f64" :background "#3c3836"))))
              `(line-number-current-line
                ((t (:foreground "#fe8019" :background "#3c3836"))))
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
              `(fringe ((t (:background "#404040"))))
              `(vertical-border ((t (:foreground "#505050"))))
              `(mode-line
                ((t (:font ,modeline-font
                           :box (:line-width -2 :color "#555555" :style nil)))))
              `(mode-line-inactive
                ((t (:font ,modeline-font
                           :box (:line-width -2 :color "#555555" :style nil))))))))
      ;; ensure powerline colors are updated
      (powerline-reset)
      (powerline-default-theme))))

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
  :pin melpa-stable
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.edn\\'" . clojure-mode))
  :config
  (let ((inhibit-message t))
    (use-package paredit)
    (ensure-lispy)
    (use-package paren-face)
    (use-package aggressive-indent)
    (use-package cider
      :pin melpa-stable
      :diminish cider-mode
      :init
      (setq clojure-use-backtracking-indent t
            clojure-indent-style 'always-align ;; 'align-arguments
            cider-repl-use-pretty-printing t
            cider-repl-popup-stacktraces t
            cider-auto-select-error-buffer t
            cider-prompt-for-symbol nil
            nrepl-use-ssh-fallback-for-remote-hosts t)
      (ensure-tramp)
      :config
      ;; (setq-default clojure-docstring-fill-column 70)
      (unless (exclude-pkg? 'auto-complete)
        (use-package ac-cider)
        (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
        (add-hook 'cider-mode-hook 'ac-cider-setup)
        (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
        (eval-after-load "auto-complete"
          `(progn
             (add-to-list 'ac-modes 'cider-mode)
             (add-to-list 'ac-modes 'cider-repl-mode))))
      (set-mode-name clojure-mode "CLJ")
      (set-mode-name clojurescript-mode "CLJS")
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
        (cider-nrepl-request:eval
         (format "(require '%s :reload)"
                 (buffer-local-value 'cider-buffer-ns (cl-first (cider-repl-buffers))))
         (lambda (_response) nil)))
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
      (add-hook 'nrepl-connected-hook 'cider-figwheel-init t)
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
      (when --use-lispy
        (enable-lispy 'clojure-mode-hook)
        (enable-lispy 'cider-mode-hook)
        (enable-lispy 'cider-repl-mode-hook))
      (define-map-keys cider-mode-map
        ("C-c C-k" 'cider-load-buffer-reload-repl)
        ("C-c n" 'cider-repl-set-ns)
        ("C-c C-p" nil))
      (define-map-keys cider-repl-mode-map
        ("C-c C-p" nil)))
    (use-package clj-refactor
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
      :disabled t
      :init (use-package flycheck)
      :config (flycheck-clojure-setup))))

(use-package slime
  :pin melpa-stable
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

  (use-package slime-annot)
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
            (unless jeff/use-global-aggressive-indent
              (use-package aggressive-indent)
              (aggressive-indent-mode))
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

(use-package web-mode
  :pin melpa-stable
  :mode "\\.js\\'" "\\.jsx\\'" "\\.json\\'"
  :config
  (use-package tern
    :disabled t
    :pin melpa-stable)
  (use-package flycheck)
  (use-package js2-mode
    :pin melpa-stable
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

(defcustom jeff/use-spaceline nil
  "Uses spaceline for modeline if non-nil."
  :group 'jeff)

(use-package powerline
  :if (not jeff/use-spaceline)
  :config
  (setq powerline-height 43
        powerline-default-separator 'arrow
        powerline-display-buffer-size nil
        powerline-display-mule-info nil
        powerline-display-hud nil
        powerline-gui-use-vcs-glyph t
        powerline-text-scale-factor 0.875)
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
  (add-hook 'after-init-hook #'doom-themes-neotree-config)
  (setq doom-neotree-enable-variable-pitch t
        doom-neotree-file-icons 'simple
        doom-neotree-line-spacing 2)
  (doom-themes-visual-bell-config)
  (when jeff/enable-auto-neotree
    (doom-themes-neotree-config)))

(use-package default-text-scale
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

(defun jeff/init-ui (&optional frame)
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

  ;;(set-frame-font "Inconsolata for Powerline:pixelsize=24")
  ;;(set-frame-font "Inconsolata for Powerline 16")
  ;;(set-frame-font "SauceCodePro Medium:pixelsize=28")
  ;;(set-frame-font "Inconsolata for Powerline:pixelsize=30")
  ;;(set-frame-font "InconsolataGo Nerd Font Mono:pixelsize=31")
  ;;(set-frame-font "Inconsolata Nerd Font Mono 15")
  ;;(set-frame-font "InconsolataGo Nerd Font Mono 15")

  (cond ((equal system-name "jeff-osx")
         (set-frame-width nil 100)
         (set-frame-height nil 48))
        ((equal system-name "jeff-mbp")
         nil))
  ;;(when (graphical?) (global-display-line-numbers-mode 1))
  (powerline-reset)
  (powerline-default-theme)
  nil)

(load-local "auto-margin")

(when (graphical?)
  ;; (add-hook 'after-init-hook 'helm-projectile-switch-project)
  (when jeff/enable-auto-neotree (use-package neotree)))

(add-hook 'post-command-hook 'force-mode-line-update)

(jeff/init-ui)
(add-hook 'after-make-frame-functions #'jeff/init-ui)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (cider sh-scriptf zenburn-theme web-mode use-package systemd spacemacs-theme smex scala-mode rainbow-mode python-mode powerline pkgbuild-mode paxedit paren-face paradox outshine org-pomodoro org-bullets nginx-mode mpv monokai-theme molokai-theme moe-theme mic-paren material-theme markdown-mode magit lispy leuven-theme jade-mode helm-projectile helm-ag groovy-mode git-gutter-fringe ghc gh-md flycheck-pos-tip flx-ido flatland-theme evil-lisp-state elisp-slime-nav doom-themes disable-mouse diminish default-text-scale cyberpunk-theme company-statistics company-quickhelp color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clj-refactor base16-theme autothemer apropospriate-theme anti-zenburn-theme all-the-icons alect-themes aggressive-indent ac-slime ac-haskell-process))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
