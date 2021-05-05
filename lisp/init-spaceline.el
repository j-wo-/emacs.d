(use-package spaceline
  :init
  (setq powerline-height 40
        powerline-default-separator 'arrow
        ;; spaceline-inflation 1.4
        spaceline-separator-dir-left '(right . right)
        spaceline-separator-dir-right '(right . right)
        spaceline-workspace-numbers-unicode t)
  :config
  (require 'spaceline-config)
  ;; (spaceline-compile)

  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (spaceline-toggle-buffer-position-on)
  (spaceline-toggle-hud-off)
  (spaceline-toggle-line-column-on)

  (defface jeff/modeline-buffer-path
    '((t (:inherit mode-line-buffer-id
                   :bold nil
                   :foreground "#a89984")))
    "Face used for the buffer name."
    :group 'jeff)

  (defun jeff/buffer-path ()
    (when (and buffer-file-name (projectile-project-p) (projectile-project-root))
      (let ((buffer-path (file-relative-name
                          (file-name-directory
                           (or buffer-file-truename (file-truename buffer-file-name)))
                          (projectile-project-root))))
        (unless (equal buffer-path "./")
          (let ((max-length (truncate (* (window-body-width) 0.4))))
            (if (> (length buffer-path) max-length)
                (let* ((path (nreverse (split-string buffer-path "/" t)))
                       ;; (path (subseq path 0 (min (length path) 2)))
                       (output ""))
                  (when (and path (equal "" (car path)))
                    (setq path (cdr path)))
                  (while (and path (<= (length output) (- max-length 4)))
                    (setq output (concat (car path) "/" output)
                          path (cdr path)))
                  (when path
                    (setq output (concat "../" output)))
                  (unless (string-suffix-p "/" output)
                    (setq output (concat output "/")))
                  output)
              buffer-path))))))

  (spaceline-define-segment
   buffer-id-with-path
   "Name of buffer (or path relative to project root)."
   (let ((name (propertize (if (buffer-file-name)
                               (file-name-nondirectory (buffer-file-name))
                             (buffer-name))
                           'face 'mode-line-buffer-id))
         (path (jeff/buffer-path)))
     (if path
         (concat (propertize path 'face
                             '(:inherit jeff/modeline-buffer-path))
                 name)
       name)))

  (defun jeff/spaceline-theme ()
    (spaceline-install
     `((((((persp-name :fallback workspace-number)
           window-number) :separator "|")
         buffer-modified
         buffer-size)
        :face highlight-face
        :priority 0)
       (anzu :priority 4)
       auto-compile
       ((buffer-id-with-path remote-host)
        :priority 5)
       major-mode
       (process :when active)
       ((flycheck-error flycheck-warning flycheck-info)
        :when active
        :priority 3)
       (minor-modes :when active)
       (mu4e-alert-segment :when active)
       (erc-track :when active)
       (version-control :when active
                        :priority 7)
       (org-pomodoro :when active)
       (org-clock :when active)
       nyan-cat)
     `(which-function
       (python-pyvenv :fallback python-pyenv)
       purpose
       (battery :when active)
       (selection-info :priority 2)
       input-method
       ((buffer-encoding-abbrev
         point-position
         line-column)
        :separator " | "
        :priority 3)
       (global :when active)
       (buffer-position :priority 0)
       (hud :priority 0)))

    (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

  ;; (spaceline-spacemacs-theme)
  ;; (spaceline-emacs-theme)
  (jeff/spaceline-theme))
