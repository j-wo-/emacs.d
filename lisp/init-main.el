;;; -*- lexical-binding: t -*-

(require 'init-base)
(require 'init-theme)
(require 'cl-lib)
(require 'use-package)

(use-package dash)

(use-package diminish
  :config
  (diminish 'eldoc-mode)
  (diminish 'isearch-mode))

(use-package vterm
  :disabled t
  :commands vterm)

(use-package outshine
  :defer t
  ;;:pin melpa
  :diminish outshine-mode
  :config
  (require 'outline)
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
  :disabled t
  ;;:pin melpa
  :config
  (evil-mode 1)
  (diminish 'undo-tree-mode)
  (use-package evil-lisp-state
    ;;:pin melpa
    :config
    (evil-lisp-state-leader ", l")))

(use-package disable-mouse
  :if nil ;; (laptop?)
  :config
  (diminish 'global-disable-mouse-mode)
  (diminish 'disable-mouse-mode)
  (diminish 'disable-mouse-global-mode)
  (disable-mouse-global-mode))

(use-package tramp
  :ensure nil
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

(set-mode-name emacs-lisp-mode "ELisp")

(defun rainbow-mode-1 () (rainbow-mode 1))

(defun use-rainbow-mode (mode &optional mode-hook)
  (use-package rainbow-mode)
  (let ((mode-hook (or mode-hook (symbol-suffix mode "-hook"))))
    (when (symbolp mode-hook)
      (add-hook mode-hook 'rainbow-mode-1))))

(use-package sh-script
  :ensure nil
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

(use-package rainbow-mode
  :defer 0.3
  :diminish rainbow-mode
  :init
  (setq rainbow-html-colors t
        rainbow-html-colors-alist nil
        rainbow-ansi-colors 'auto
        rainbow-x-colors nil
        rainbow-latex-colors nil
        rainbow-r-colors nil)
  :config
  (use-package sh-script)
  (dolist (mode '(sh-mode shell-mode))
    (use-rainbow-mode mode)))

(use-package cc-mode
  :ensure nil
  :mode (("\\.c\\'"     . c-mode)
         ("\\.cc\\'"    . c++-mode)
         ("\\.cpp\\'"   . c++-mode)
         ("\\.java\\'"  . c++-mode))
  :config
  (--indent-tabs-mode 'c-mode-hook t)
  (--indent-tabs-mode 'c++-mode-hook t)
  (--indent-tabs-mode 'objc-mode-hook t)
  (--indent-tabs-mode 'java-mode-hook t))

(use-package python-mode
  ;;:pin melpa-stable
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
  ;;:pin melpa
  :defer t
  :diminish yas-minor-mode)

(defvar --use-global-aggressive-indent t)

(defun --load-aggressive-indent ()
  (add-to-list 'load-path "~/.emacs.d/aggressive-indent-mode")
  (require 'aggressive-indent)
  ;; " " " "
  ;; unicode 2004 U+2004 (thick space)
  (use-package diminish)
  (diminish 'aggressive-indent-mode (if (graphical?) " " " Aggr"))
  (setq aggressive-indent-sit-for-time 0.025)
  ;; 'aggressive-indent-dont-indent-if
  (when --use-global-aggressive-indent
    (dolist (mode '(cider-repl-mode c-mode c++-mode objc-mode java-mode))
      (add-to-list 'aggressive-indent-excluded-modes mode))
    (aggressive-indent-global-mode)))

(--load-aggressive-indent)

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
  ;;:pin melpa
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
    ;; (setq company-quickhelp-use-propertized-text t)
    (company-quickhelp-mode 1))
  (global-company-mode 1))

(defcustom jeff/use-projectile-ag nil
  "Use ag for projectile search if non-nil; otherwise use git grep."
  :group 'jeff)

(defvar jeff/projectile-search-fn
  (if jeff/use-projectile-ag 'helm-projectile-ag 'helm-projectile-grep))

(use-package projectile
  :defer 0.25
  :init
  (setq projectile-use-git-grep t
        projectile-switch-project-action 'helm-projectile
        projectile-indexing-method 'hybrid
        projectile-enable-caching t
        projectile-mode-line-prefix "")
  :bind (("s-p"         . helm-projectile)
         ("C-c p"       . helm-projectile)
         ("C-c C-p"     . helm-projectile)
         ("C-c TAB"     . helm-projectile-switch-to-buffer)
         ("C-."         . helm-projectile-grep)
         ("C-c C-g"     . helm-projectile-grep)
         ("C-c g"       . helm-projectile-grep)
         ("M-/"         . helm-projectile-grep)
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

(use-package flycheck
  ;;:pin melpa-stable
  :defer 0.2
  :diminish " F "
  :init
  (setq flycheck-global-modes '(not org-mode js-mode)
        ;; '(clojure-mode clojurec-mode clojurescript-mode groovy-mode)
        flycheck-disabled-checkers '(clojure-cider-typed
                                     clojure-cider-kibit
                                     clojure-cider-eastwood
                                     emacs-lisp-checkdoc)
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
  (global-flycheck-mode 1))

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
  :defer 0.25
  :config (paren-activate))

(use-package paren-face
  :defer 0.5
  :config
  (setq paren-face-regexp "[\\(\\)]")
  (global-paren-face-mode)
  (--set-paren-face-colors)
  (defconst clojure-brackets-keywords
    '(("\\[" 0 'square-brackets)
      ("\\]" 0 'square-brackets)
      ("[\\{\\}]" 0 'curly-brackets)))
  (defun --custom-paren-face-mode-hook ()
    (if paren-face-mode
        (font-lock-add-keywords nil clojure-brackets-keywords t)
      (font-lock-remove-keywords nil clojure-brackets-keywords))
    (when (called-interactively-p 'any)
      (font-lock-ensure)))
  (add-hook 'paren-face-mode-hook '--custom-paren-face-mode-hook))

(use-package paredit
  :defer t
  :diminish (paredit-mode . "()")
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
    ("C-M-f"        nil)
    ("M-s"          nil)))

(use-package smartparens
  ;;:pin melpa-stable
  :defer 0.2
  :config
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  (diminish 'smartparens-mode " SP")
  (when nil
    (global-set-key (kbd "C-{")
                    (lambda (&optional _arg)
                      (interactive "P")
                      (sp-wrap-with-pair "(")))
    (global-set-key (kbd "C-(")
                    (lambda (&optional _arg)
                      (interactive "P")
                      (sp-wrap-with-pair "["))))
  (smartparens-global-mode t))

(defun activate-paxedit ()
  (use-package paxedit)
  (paxedit-mode 1))

(use-package paxedit
  :defer t
  :diminish ""
  ;;:pin melpa
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

(use-package with-editor
  :defer t
  ;;:pin melpa
  )

(use-package magit
  ;;:pin melpa
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-dispatch-popup))
  :config
  (diminish 'auto-revert-mode)
  (define-map-keys magit-status-mode-map
    ("C-<tab>" nil))
  (use-package projectile))

(use-package git-timemachine
  :bind (("s-g" . git-timemachine)
         ("H-s-g" . git-timemachine)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode))

(use-package easy-kill
  :config
  (define-map-keys global-map
    ("C-S-k" 'easy-kill)))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'whitespace-mode))
  (add-hook 'before-save-hook 'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package paradox
  :defer t
  ;;:pin melpa
  :commands list-packages paradox-list-packages
  :config (let ((inhibit-message t))
            (paradox-enable)))

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
  ;;:pin melpa
  :commands markdown-mode gfm-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config (use-package gh-md))

(use-package nginx-mode
  :mode "/nginx.conf$" "\\.nginx-site\\'")

(use-package alert
  ;;:pin melpa
  :if (--window-system-available)
  :config
  (setq alert-default-style (if (graphical?) 'libnotify 'message)
        alert-fade-time 4))

(defun jeff/load-org-notify ()
  (interactive)
  (when (not (featurep 'org-notify))
    (use-package org)
    (add-to-list 'load-path "~/.emacs.d/org-notify")
    (require 'org-notify)
    (setq org-notify-interval 600
          org-notify-fade-time 7)))

'(use-package org
   :mode ("\\.org\\'" . org-mode)
   :bind (("C-c l" . org-store-link)
          ("C-c a" . org-agenda)
          ("C-c c" . org-capture))
   :config
   (setq org-log-done 'time
         org-agenda-files '("~/org/self.org"
                            "~/org/schedule.org"
                            "~/org/work.org"
                            "~/org/sysrev.org")
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
     ("M-s d"          'org-deadline))
   (define-map-keys-multi (global-map org-mode-map)
     ("M-s p"          'org-pomodoro)
     ("M-s c"          'org-notify-check))
   (use-package org-ql)
   (use-package org-present)
   (use-package org-projectile)
   (use-package helm-org-rifle)
   (use-package org-super-agenda)
   (use-package org-gcal)
   (use-package org-fancy-priorities)
   (jeff/load-org-notify)
   (use-package org-alert
     :disabled t
     :load-path "~/.emacs.d/org-alert"
     :config
     (setq org-alert-interval 600
           org-alert-fade-time 10)
     (org-alert-disable))
   (use-package org-bullets
     ;;:pin melpa
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
  ;;:pin melpa
  :diminish cider-mode
  :commands sysrev stop-cider-all --cider-quit-all --benchmark-sysrev
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
  :config
  (use-package tramp)
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
    (use-package smartparens)
    (dolist (m '(clojure-mode clojurescript-mode cider-repl-mode))
      (add-to-list 'sp-ignore-modes-list m))
    (dolist (mh '(clojure-mode-hook clojurescript-mode-hook))
      (add-hook mh 'cider-mode))
    (dolist (mh '(clojure-mode-hook clojurescript-mode-hook cider-repl-mode-hook))
      (add-hook mh 'enable-paredit-mode)
      (add-hook mh 'paxedit-mode))
    (defun --cider-reload-repl-ns ()
      (let ((ns (buffer-local-value 'cider-buffer-ns (cl-first (cider-repl-buffers)))))
        (when ns
          (cider-nrepl-request:eval
           (format "(require '%s :reload)" ns)
           (lambda (_response) nil)))))
    (add-hook 'cider-file-loaded-hook '--cider-reload-repl-ns)
    (when --use-lispy
      (enable-lispy 'clojure-mode-hook)
      (enable-lispy 'cider-mode-hook)
      (enable-lispy 'cider-repl-mode-hook))
    (define-map-keys cider-mode-map
      ("C-c C-k" 'cider-load-buffer)
      ("C-c n" 'cider-repl-set-ns)
      ("C-c C-p" nil))
    (define-map-keys cider-repl-mode-map
      ("C-c C-p" nil)
      ("C-M-l" 'cider-repl-clear-buffer))
    (require 'auto-cider)))

(use-package clj-refactor
  :defer t
  ;;:pin melpa
  :diminish clj-refactor-mode
  :config
  (setq cljr-warn-on-eval nil
        cljr-suppress-middleware-warnings nil)
  (defun clj-refactor-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1)  ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (add-hook 'clojure-mode-hook #'clj-refactor-clojure-mode-hook)
  (add-hook 'clojurescript-mode-hook #'clj-refactor-clojure-mode-hook))

(use-package flycheck-clojure
  ;;:pin melpa-stable
  :defer t
  ;; :disabled t
  :config
  (use-package flycheck)
  (flycheck-clojure-setup))

(use-package flycheck-clj-kondo
  ;;:pin melpa-stable
  :defer t
  :config
  (use-package flycheck)
  (require 'flycheck-clj-kondo))

(use-package clojure-mode
  ;;:pin melpa-stable
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
  ;;:pin melpa
  :commands slime
  :mode (("\\.lisp\\'" . lisp-mode)
         ("\\.asd\\'" . lisp-mode))
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

  (dolist (mh '(lisp-mode-hook slime-mode-hook slime-repl-mode-hook))
    ;;(enable-lispy 'lisp-mode-hook)
    (add-hook mh 'turn-off-smartparens-mode)
    (add-hook mh 'enable-paredit-mode))

  ;;(use-package slime-company)
  (use-package ac-slime
    :config
    (defun set-up-slime-ac-fuzzy () (set-up-slime-ac t))
    (add-hook 'slime-mode-hook 'set-up-slime-ac-fuzzy)
    (add-hook 'slime-repl-mode-hook 'set-up-slime-ac-fuzzy)))

;; using hook this way to avoid infinite loop in melpa bootstrap
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
  :mode (("\\.scala\\'" . scala-mode)
         ("\\.sbt\\'" . scala-mode))
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
  :disabled t
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

(use-package less-css-mode
  ;;:pin melpa
  :mode ("\\.less\\'" "\\.variables\\'" "\\.overrides\\'")
  :config (use-rainbow-mode 'less-css-mode))

(use-package js2-mode
  :defer t
  ;;:pin melpa
  :mode ("\\.js\\'" "\\.json\\'" "\\.config/waybar/config\\'")
  :config
  (require 'flycheck)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)
  (setq js2-include-node-externs t
        js2-include-browser-externs t
        js2-strict-trailing-comma-warning nil
        js2-indent-switch-body t
        js2-basic-offset 2)
  (defun --custom-js2-mode-hook ()
    (setq-local js2-basic-offset 2)
    ;;(tern-mode t)
    (when (executable-find "eslint")
      (flycheck-select-checker 'javascript-eslint)))
  (add-hook 'js2-mode-hook '--custom-js2-mode-hook)
  (add-hook 'js2-jsx-mode-hook '--custom-js2-mode-hook))

(use-package js
  :ensure nil
  :mode (("\\.js\\'"                      . js-mode)
         ("\\.json\\'"                    . js-mode)
         ("\\.config/waybar/config\\'"    . js-mode))
  :init (setq js-indent-level 2))

(use-package web-mode
  ;;:pin melpa
  ;; :mode ("\\.js\\'" "\\.jsx\\'" "\\.json\\'")
  :mode "\\.jsx\\'"
  :config
  (use-package tern
    :disabled t
    ;;:pin melpa
    )
  (use-package flycheck)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
  (add-to-list 'flycheck-disabled-checkers 'json-jsonlist)
  (defun --custom-web-mode-hook ()
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (flycheck-mode 1)
    ;; (tern-mode t)
    (when (executable-find "eslint")
      (flycheck-select-checker 'javascript-eslint)))
  (add-hook 'web-mode-hook '--custom-web-mode-hook))

(use-package jade-mode :mode "\\.jade\\'")

(use-package all-the-icons :if jeff/enable-auto-neotree)
;; (all-the-icons-install-fonts)

(use-package doom-themes
  :config
  ;;(add-hook 'after-init-hook #'doom-themes-visual-bell-config)
  ;;(add-hook 'after-init-hook #'doom-themes-neotree-config)
  (setq doom-neotree-enable-variable-pitch t
        doom-neotree-file-icons 'simple
        doom-neotree-line-spacing 2)
  ;;(doom-themes-visual-bell-config)
  (when jeff/enable-auto-neotree
    (doom-themes-neotree-config)))

(use-package default-text-scale
  :defer 0.4
  :config
  (setq default-text-scale-amount 10)
  (default-text-scale-mode)
  (define-map-keys global-map
    ("C-x =" 'default-text-scale-increase)
    ("C-x -" 'default-text-scale-decrease)
    ("C-x 0" 'default-text-scale-reset)))

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

(provide 'init-main)
