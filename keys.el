(defun vi-open-next-line (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (indent-according-to-mode))

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

(global-set-key "\C-l" 'recenter-top-bottom-refresh)

(global-set-key [(control o)] 'vi-open-next-line)
(global-set-key [C-down] 'scroll-down-one-line)
(global-set-key [C-up] 'scroll-up-one-line)
(global-set-key "\C-f" 'forward-sexp)
(global-set-key "\C-b" 'backward-sexp)
(global-set-key "\C-t" 'transpose-sexps)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-\M-q" 'indent-sexp)
(global-set-key "\C-\M-k" 'kill-sexp)
;; (global-set-key "\C-\M-a" 'lispy-beginning-of-defun)

;; (global-set-key (kbd "C-{") (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "(")))
;; (global-set-key (kbd "C-(") (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "[")))

(define-key key-translation-map (kbd "(") (kbd "["))
(define-key key-translation-map (kbd ")") (kbd "]"))
(define-key key-translation-map (kbd "[") (kbd "("))
(define-key key-translation-map (kbd "]") (kbd ")"))

;; Note: the following still breaks middle-click pasting
;;(define-key paredit-mode-map (kbd "[") 'paredit-open-round)
;;(define-key paredit-mode-map (kbd "]") 'paredit-close-round)
;;(define-key paredit-mode-map (kbd "(") 'paredit-open-square)
;;(define-key paredit-mode-map (kbd ")") 'paredit-close-square)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
