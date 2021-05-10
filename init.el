;;; -*- lexical-binding: t -*-

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(set-language-environment "utf-8")

(defvar --run-profiler nil)
(setq use-package-compute-statistics nil)

(when --run-profiler
  (require 'profiler)
  (setq profiler-sampling-interval 100000)
  (profiler-start 'cpu))

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'init-base)
(load-local 'init-use-package)
(require 'use-package)
;;(setq use-package-compute-statistics t)
;; make sure deferred packages are still installed on initial bootstrap
(ensure-installed nil
  rainbow-mode)
(ensure-installed melpa
  with-editor
  js2-mode
  ample-theme
  flatland-theme
  gh-md
  mic-paren
  molokai-theme
  company
  company-quickhelp
  company-statistics
  popup
  pos-tip
  bind-key
  all-the-icons)
(ensure-installed melpa-stable
  ac-cider
  ace-window
  alert
  ;; all-the-icons
  ;; ample-theme
  anti-zenburn-theme
  anzu
  apropospriate-theme
  async
  auto-complete
  autothemer
  avy
  base16-theme
  ;; bind-key
  cider
  clj-refactor
  clojure-mode
  color-theme-sanityinc-solarized
  ;; color-theme-sanityinc-tomorrow
  ;; company
  ;; company-quickhelp
  ;; company-statistics
  counsel
  cyberpunk-theme
  dash
  default-text-scale
  diminish
  disable-mouse
  doom-themes
  easy-kill
  elisp-slime-nav
  epl
  expand-region
  ;; flatland-theme
  flx
  flx-ido
  flycheck
  flycheck-clj-kondo
  flycheck-clojure
  flycheck-pos-tip
  fringe-helper
  ;; gh-md
  git-commit
  git-gutter
  git-gutter-fringe
  git-timemachine
  gntp
  groovy-mode
  helm
  helm-ag
  helm-core
  helm-projectile
  hydra
  iedit
  inflections
  ivy
  jade-mode
  ;; js2-mode
  leuven-theme
  lispy
  log4e
  lv
  magit
  markdown-mode
  material-theme
  ;; mic-paren
  moe-theme
  ;; molokai-theme
  monokai-theme
  mpv
  multiple-cursors
  nginx-mode
  org-bullets
  org-pomodoro
  outorg
  outshine
  paradox
  paredit
  paren-face
  parseclj
  parseedn
  paxedit
  pkgbuild-mode
  pkg-info
  ;; popup
  ;; pos-tip
  powerline
  projectile
  python-mode
  queue
  ;; rainbow-mode
  s
  scala-mode
  sesman
  smartparens
  smex
  spinner
  swiper
  systemd
  transient
  use-package
  ;; vterm
  web-mode
  ;; with-editor
  yaml-mode
  yasnippet
  zenburn-theme
  zoutline)
(load-local 'init-base)
(load-local 'init-theme)
(load-local 'init-keys)
(load-local 'init-commands)
(load-local 'init-main)
(load-local 'init-configure)
(load-local 'init-copy-paste)
(load-local 'init-launch)
(load-local 'auto-margin)

(when --run-profiler
  (add-to-list 'after-init-hook (lambda ()
                                  (profiler-stop)
                                  (profiler-report))))

(when use-package-compute-statistics
  (use-package-report))
