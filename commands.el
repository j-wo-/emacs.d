(defun kill-mode-buffers (mode)
  (interactive
   (list (read--expression "Mode: ")))
  (--each (buffer-list)
    (with-current-buffer it
      (when (eql major-mode mode)
        (kill-buffer it)))))

(defun kill-image-buffers ()
  (interactive)
  (kill-mode-buffers 'image-mode))

(defun current-file-extension ()
  (file-name-extension (buffer-file-name (current-buffer))))

(defun fully-indent-file (path)
  (save-excursion
    (with-current-buffer (find-file-noselect path)
      (indent-region (point-min) (point-max))
      (save-buffer)
      (buffer-file-name))))

(defun indent-directory-files ()
  (interactive)
  (let* ((ext (read-from-minibuffer
               "File extension: " (current-file-extension)))
         (ext-re (if (equal ext "") "" (format "\\.%s$" ext))))
    (--each (directory-files default-directory t ext-re)
      (fully-indent-file it))))

(defun nxml-pretty-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max)
                             "xmllint --format -" (buffer-name) t)
    (nxml-mode)
    (indent-region (point-min) (point-max))))
