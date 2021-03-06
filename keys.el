(defmacro define-map-keys (map &rest defs)
  `(progn
     ,@(mapcar (lambda (entry)
                 (cl-destructuring-bind (kbd-str func) entry
                   `(define-key ,map (kbd ,kbd-str) ,func)))
               defs)))

(defun jeff/scroll-down-one-line ()
  (interactive)
  (scroll-up 1)
  ;; (next-line)
  (forward-line 1))

(defun jeff/scroll-up-one-line ()
  (interactive)
  (scroll-down 1)
  ;; (previous-line)
  (forward-line -1))

(defun jeff/helm-buffers-list-all ()
  (interactive)
  (let ((helm-boring-buffer-regexp-list
         '("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf")))
    (helm-buffers-list)))

(define-map-keys global-map
  ;; redefine bad default bindings
  ("C-o"          'other-window)
  ("C-1"          'delete-other-windows)
  ("C-x 1"        'delete-other-windows) ; terminal
  ("C-2"          'delete-other-windows-vertically)
  ("C-M-2"        'delete-other-windows-vertically)
  ("C-x 2"        'delete-other-windows-vertically) ; terminal
  ("C-q"          'delete-window)
  ("C-x x"        'split-window-below)
  ("C-<tab>"      'helm-mini)
  ("C-x TAB"      'helm-mini)           ; terminal
  ("C-c TAB"      'helm-projectile-switch-to-buffer)
  ("M-<tab>"      'helm-projectile-switch-to-buffer)
  ("C-x s"        'save-buffer)
  ("C-x C-s"      'save-buffer)
  ("C-x f"        'helm-find-files)
  ("C-x C-f"      'helm-find-files)
  ("C-x F"        'find-file)
  ;; unset defaults
  ("C-x o"        nil)                  ; other-window
  ;; ("C-x 0"        nil)                  ; delete-window
  ("C-x 3"        nil)                  ; split-window-right
  ("C-x b"        nil)                  ; ido-switch-buffer
  ;; custom bindings
  ("C-<left>"     'previous-buffer)
  ("C-<right>"    'next-buffer)
  ("M-<left>"     'previous-buffer)
  ("M-<right>"    'next-buffer)
  ("C-x k"        'kill-this-buffer)
  ("C-x b"        'helm-buffers-list)
  ("C-x B"        'jeff/helm-buffers-list-all)
  ("C-l"          'recenter-top-bottom)
  ("C-<down>"     'jeff/scroll-down-one-line)
  ("C-<up>"       'jeff/scroll-up-one-line)
  ("C-f"          'forward-sexp)
  ("C-b"          'backward-sexp)
  ("C-t"          'transpose-sexps)
  ("C-w"          'backward-kill-word)
  ("C-x C-k"      'kill-region)
  ("M-q"          'indent-sexp)
  ("C-M-k"        'kill-sexp)
  ("C-M-w"        'split-window-auto)
  ("C-M-s"        'split-window-auto)
  ("C-M-f"        'toggle-frame-fullscreen)
  ("C-x n"        'jeff/neotree-project-dir)
  ("C-c ;"        'comment-or-uncomment-region))

;; swap () and [] keys
(when t ;; (graphical?)
  (define-key key-translation-map (kbd "(") (kbd "["))
  (define-key key-translation-map (kbd ")") (kbd "]"))
  (define-key key-translation-map (kbd "[") (kbd "("))
  (define-key key-translation-map (kbd "]") (kbd ")")))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defvar mac-command-modifier)
(defvar mac-option-modifier)

;; set modifier keys for MacOS
(when (mac?)
  (cond ((or (and (equal (system-name) "jeff-m1.lan") (gui-emacs-mac?))
             (and (equal (system-name) "jeff-mbp") (gui-mac-std?)))
         (progn (setq mac-command-modifier 'meta)
                (setq mac-option-modifier 'super)))
        (t
         (progn (setq mac-command-modifier 'super)
                (setq mac-option-modifier 'meta)))))
