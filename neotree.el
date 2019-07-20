(defvar jeff/neotree-font-set nil)

(defun jeff/init-neotree-font ()
  (when (null jeff/neotree-font-set)
    (set-face-attribute 'variable-pitch nil
                        :font (font-spec :family "Fira Sans" :size 14))
    (setq jeff/neotree-font-set t)))

(defun jeff/on-switch-project ()
  (remove-frame-margins)
  (neotree-projectile-action)
  (autoset-frame-margins)
  (helm-projectile))

(defun jeff/neotree-buffer ()
  (let ((result nil))
    (mapcar (lambda (window)
              (let ((buffer (window-buffer window)))
                (save-excursion
                  (with-current-buffer buffer
                    (when (eq major-mode 'neotree-mode)
                      (setq result buffer))))))
            (window-list))
    result))

(defun jeff/neotree-open-p ()
  (if (jeff/neotree-buffer) t nil))

(defun jeff/neotree-expanded-dirs ()
  (let ((buffer (jeff/neotree-buffer))
        (result nil))
    (when buffer
      (save-excursion
        (with-current-buffer buffer
          (setq result neo-buffer--expanded-node-list)))
      result)))

(defun jeff/neotree-unexpand-dir (dir)
  (let ((buffer (jeff/neotree-buffer)))
    (when buffer
      (save-excursion
        (with-current-buffer buffer
          (setq neo-buffer--expanded-node-list
                (remove dir neo-buffer--expanded-node-list)))))))

(defvar jeff/neotree-latest-file nil)
(defvar jeff/neotree-temp-dirs nil)

(defun jeff/update-neotree-file ()
  (when (jeff/neotree-open-p)
    (let ((file (buffer-file-name)))
      (when (and file (not (equal file jeff/neotree-latest-file)))
        (if (and jeff/neotree-latest-file
                 (equal (file-name-directory file)
                        (file-name-directory jeff/neotree-latest-file)))
            ;; directory hasn't changed
            (progn
              (setq jeff/neotree-latest-file file)
              (neotree-refresh t))
          (let ((temp-dirs jeff/neotree-temp-dirs))
            (setq jeff/neotree-latest-file file)
            ;; unexpand directories in neotree buffer
            (dolist (dir temp-dirs)
              (jeff/neotree-unexpand-dir dir))
            ;; record temp directories expanded by this action
            (let* ((pre-dirs (jeff/neotree-expanded-dirs))
                   (_refresh (neotree-refresh t))
                   (post-dirs (jeff/neotree-expanded-dirs))
                   (new-dirs (remove-if (lambda (d)
                                          (member d pre-dirs))
                                        post-dirs)))
              (setq jeff/neotree-temp-dirs new-dirs))
            (neotree-refresh t)))))))

(defun jeff/do-neotree-toggle ()
  (interactive)
  (jeff/init-neotree-font)
  (remove-frame-margins)
  (neotree-toggle)
  (autoset-frame-margins))

(defun jeff/neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (use-package projectile)
  (jeff/init-neotree-font)
  (let ((project-dir (and (projectile-project-p)
                          (projectile-project-root)))
        (file-name (buffer-file-name)))
    (jeff/do-neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root"))))

(use-package neotree
  :commands neotree-toggle jeff/do-neotree-toggle jeff/neotree-project-dir
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
  (when jeff/enable-auto-neotree
    (setq projectile-switch-project-action 'jeff/on-switch-project)
    (add-hook 'post-command-hook 'jeff/update-neotree-file)))
