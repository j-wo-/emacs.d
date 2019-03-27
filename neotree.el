(defvar jeffwk/neotree-font-set nil)
(defun jeffwk/init-neotree-font ()
  (when (null jeffwk/neotree-font-set)
    (set-face-attribute 'variable-pitch nil
                        :font (font-spec :family "Fira Sans" :size 14))
    (setq jeffwk/neotree-font-set t)))

(defun jeffwk/on-switch-project ()
  (remove-frame-margins)
  (neotree-projectile-action)
  (autoset-frame-margins)
  (helm-projectile))

(defun jeffwk/neotree-buffer ()
  (let ((result nil))
    (mapcar (lambda (window)
              (let ((buffer (window-buffer window)))
                (save-excursion
                  (with-current-buffer buffer
                    (when (eq major-mode 'neotree-mode)
                      (setq result buffer))))))
            (window-list))
    result))

(defun jeffwk/neotree-open-p ()
  (if (jeffwk/neotree-buffer) t nil))

(defun jeffwk/neotree-expanded-dirs ()
  (let ((buffer (jeffwk/neotree-buffer))
        (result nil))
    (when buffer
      (save-excursion
        (with-current-buffer buffer
          (setq result neo-buffer--expanded-node-list)))
      result)))

(defun jeffwk/neotree-unexpand-dir (dir)
  (let ((buffer (jeffwk/neotree-buffer)))
    (when buffer
      (save-excursion
        (with-current-buffer buffer
          (setq neo-buffer--expanded-node-list
                (remove dir neo-buffer--expanded-node-list)))))))

(defvar jeffwk/neotree-latest-file nil)
(defvar jeffwk/neotree-temp-dirs nil)

(defun jeffwk/update-neotree-file ()
  (when (jeffwk/neotree-open-p)
    (let ((file (buffer-file-name)))
      (when (and file (not (equal file jeffwk/neotree-latest-file)))
        (if (and jeffwk/neotree-latest-file
                 (equal (file-name-directory file)
                        (file-name-directory jeffwk/neotree-latest-file)))
            ;; directory hasn't changed
            (progn
              (setq jeffwk/neotree-latest-file file)
              (neotree-refresh t))
          (let ((temp-dirs jeffwk/neotree-temp-dirs))
            (setq jeffwk/neotree-latest-file file)
            ;; unexpand directories in neotree buffer
            (dolist (dir temp-dirs)
              (jeffwk/neotree-unexpand-dir dir))
            ;; record temp directories expanded by this action
            (let* ((pre-dirs (jeffwk/neotree-expanded-dirs))
                   (_refresh (neotree-refresh t))
                   (post-dirs (jeffwk/neotree-expanded-dirs))
                   (new-dirs (remove-if (lambda (d)
                                          (member d pre-dirs))
                                        post-dirs)))
              (setq jeffwk/neotree-temp-dirs new-dirs))
            (neotree-refresh t)))))))

(defun jeffwk/do-neotree-toggle ()
  (interactive)
  (jeffwk/init-neotree-font)
  (remove-frame-margins)
  (neotree-toggle)
  (autoset-frame-margins))

(defun jeffwk/neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (use-package projectile)
  (jeffwk/init-neotree-font)
  (let ((project-dir (and (projectile-project-p)
                          (projectile-project-root)))
        (file-name (buffer-file-name)))
    (jeffwk/do-neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root"))))

(define-key global-map "\C-xn" 'jeffwk/neotree-project-dir)

(use-package neotree
  :commands
  (neotree-toggle
   jeffwk/do-neotree-toggle
   jeffwk/neotree-project-dir)
  :config
  (setq neo-create-file-auto-open nil
        neo-auto-indent-point nil
        neo-autorefresh nil
        neo-mode-line-type nil
        neo-show-updir-line nil
        neo-theme 'nerd
        neo-window-width 28
        neo-banner-message nil
        neo-confirm-create-file #'off-p
        neo-confirm-create-directory #'off-p
        neo-show-hidden-files nil
        neo-keymap-style 'concise
        neo-hidden-regexp-list
        '(;; vcs folders
          "^\\.\\(git\\|hg\\|svn\\)$"
          ;; compiled files
          "\\.\\(pyc\\|o\\|elc\\|lock\\|css.map\\)$"
          ;; generated files, caches or local pkgs
          "^\\(node_modules\\|vendor\\|target\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
          ;; org-mode folders
          "^\\.\\(sync\\|export\\|attach\\)$"
          "~$"
          "^#.*#$"))
  (when jeffwk/enable-auto-neotree
    (setq projectile-switch-project-action 'jeffwk/on-switch-project)
    (add-hook 'post-command-hook 'jeffwk/update-neotree-file)))
