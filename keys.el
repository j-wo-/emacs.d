(defun define-lisp-keys (mode-map)
  (dolist (entry
	    `((,(kbd "[") paredit-open-round)
	      (,(kbd "]") paredit-close-round)
	      (,(kbd "(") paredit-open-square)
	      (,(kbd ")") paredit-close-square)
	      (,(kbd "C-M-q") indent-sexp)
	      ;;(,(kbd "(") ,(lambda () (interactive) (insert "[")))
	      ;;(,(kbd ")") ,(lambda () (interactive) (insert "]")))
	
	      ;;(,(kbd "C-f") forward-sexp)
	      ;;(,(kbd "C-b") backward-sexp)
	
	      ;;(,(kbd "C-M-t") transpose-chars)
	      ;;(,(kbd "C-M-b") backward-char)
	      ;;(,(kbd "C-M-f") forward-char)
	      (,[(control ?\{)] (lambda () (interactive) (insert "(")))
	      (,[(control ?\})] (lambda () (interactive) (insert ")")))))
    (destructuring-bind (key cmd) entry
      (define-key mode-map key cmd))))

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

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
