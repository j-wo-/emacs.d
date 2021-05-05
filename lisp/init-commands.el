;;; -*- lexical-binding: t -*-

(require 'init-base)
(require 'cl-lib)
(require 'use-package)
(use-package dash)

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

(defun --align-regexp (beg end regexp &optional group spacing repeat)
  "Modified version of `align-regexp`, changed to always prompt
interactively for spacing value."
  (interactive
   (append
    (list (region-beginning) (region-end))
    (if current-prefix-arg
        (list (read-string "Complex align using regexp: "
                           "\\(\\s-*\\)" 'align-regexp-history)
              (string-to-number
               (read-string
                "Parenthesis group to modify (justify if negative): " "1"))
              (string-to-number
               (read-string "Amount of spacing (or column if negative): "
                            (number-to-string align-default-spacing)))
              (y-or-n-p "Repeat throughout line? "))
      (list (concat "\\(\\s-*\\)"
                    (read-string "Align regexp: "))
            1
            (string-to-number
             (read-string "Amount of spacing (or column if negative): "
                          (number-to-string align-default-spacing)))
            nil))))
  (align-regexp beg end regexp group spacing repeat))

(provide 'init-commands)

;; Local Variables:
;; byte-compile-warnings: (not free-vars make-local callargs)
;; End:
