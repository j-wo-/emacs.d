(defun kill-mode-buffers (mode)
  (interactive
   (list (read--expression "Mode: ")))
  (dolist (buffer (buffer-list))
    (save-excursion
      (set-buffer buffer)
      (when (eql major-mode mode)
	(kill-buffer buffer)))))

(defun kill-image-buffers ()
  (interactive)
  (kill-mode-buffers 'image-mode))
