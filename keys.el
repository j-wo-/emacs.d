(defun scroll-down-one-line ()
  (interactive)
  (scroll-up 1)
  (forward-line 1))

(defun scroll-up-one-line ()
  (interactive)
  (scroll-down 1)
  (forward-line -1))

(defun recenter-top-bottom-refresh (&optional arg)
  (interactive)
  (recenter-top-bottom arg)
  (redraw-display))

(defun jeffwk/forward-sexp ()
  (interactive)
  (forward-sexp)
  (redraw-modeline))
(defun jeffwk/backward-sexp ()
  (interactive)
  (backward-sexp)
  (redraw-modeline))

;; redefine bad default bindings
(define-key global-map (kbd "C-o") 'other-window)
(define-key global-map (kbd "C-1") 'delete-other-windows)
(define-key global-map (kbd "C-x 1") 'delete-other-windows) ;; terminal
(define-key global-map (kbd "C-2") 'delete-other-windows-vertically)
(define-key global-map (kbd "C-x 2") 'delete-other-windows-vertically) ;; terminal
(define-key global-map (kbd "C-q") 'delete-window)
(define-key global-map (kbd "C-x x") 'split-window-below)
(define-key global-map (kbd "C-<tab>") 'helm-mini)
(define-key global-map (kbd "C-x TAB") 'helm-mini) ;; terminal
(define-key global-map (kbd "C-x s") 'save-buffer)
(define-key global-map (kbd "C-x C-s") 'save-buffer)
(define-key global-map (kbd "C-x f") 'helm-find-files)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
;; unset defaults
(define-key global-map (kbd "C-x o") nil) ;; 'other-window
(define-key global-map (kbd "C-x 0") nil) ;; 'delete-window
(define-key global-map (kbd "C-x 3") nil) ;; 'split-window-right
(define-key global-map (kbd "C-x b") nil) ;; 'ido-switch-buffer

(define-key global-map (kbd "C-<left>") 'previous-buffer)
(define-key global-map (kbd "C-<right>") 'next-buffer)
(define-key global-map (kbd "C-x k") 'kill-this-buffer)
(define-key global-map (kbd "C-l") 'recenter-top-bottom-refresh)
(define-key global-map (kbd "C-<down>") 'scroll-down-one-line)
(define-key global-map (kbd "C-<up>") 'scroll-up-one-line)
(define-key global-map (kbd "C-f") 'jeffwk/forward-sexp)
(define-key global-map (kbd "C-b") 'jeffwk/backward-sexp)
(define-key global-map (kbd "C-t") 'transpose-sexps)
(define-key global-map (kbd "C-w") 'backward-kill-word)
(define-key global-map (kbd "C-x C-k") 'kill-region)
(define-key global-map (kbd "M-q") 'indent-sexp)
(define-key global-map (kbd "C-M-k") 'kill-sexp)
(define-key global-map (kbd "C-M-w") 'split-window-prefer-horizontal)
(define-key global-map (kbd "C-M-s") 'split-window-prefer-horizontal)
(define-key global-map (kbd "C-M-f") 'toggle-frame-fullscreen)
(define-key global-map (kbd "M-<right>") 'switch-to-next-buffer)
(define-key global-map (kbd "M-<left>") 'switch-to-prev-buffer)
(when (and (eql system-type 'darwin)
           (equal system-name "jeff-mbp"))
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))

;; swap () and [] keys
(define-key key-translation-map (kbd "(") (kbd "["))
(define-key key-translation-map (kbd ")") (kbd "]"))
(define-key key-translation-map (kbd "[") (kbd "("))
(define-key key-translation-map (kbd "]") (kbd ")"))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
