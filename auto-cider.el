;;; -*- lexical-binding: t -*-

(eval-when-compile
  (use-package cider))

(require 'cl-lib)

(defun all-sesman-sessions ()
  (sesman-sessions (sesman--system) t))

(defun test-buffer-name (buf regexp &optional exclude-regexp)
  (and (string-match regexp (buffer-name buf))
       (if (null exclude-regexp) t
         (not (string-match exclude-regexp (buffer-name buf))))))

(defun match-buffer-name (regexp &optional exclude-regexp)
  (cl-remove-if-not (lambda (buf)
                      (test-buffer-name buf regexp exclude-regexp))
                    (buffer-list)))

(defun match-sesman-session (regexp &optional exclude-regexp)
  (cl-first
   (cl-remove-if (lambda (ses)
                   (not (test-buffer-name (cl-second ses) regexp exclude-regexp)))
                 (all-sesman-sessions))))

(defun stop-cider-all ()
  (interactive)
  (dolist (buf (match-buffer-name "\*cider-repl\ .*"))
    (save-excursion
      (switch-to-buffer buf)
      (cider-quit))))

(defun wait-on-condition (ready-p on-ready interval max-attempts)
  (cond ((< max-attempts 1) nil)
        ((funcall ready-p) (funcall on-ready))
        (t (run-with-timer
            interval nil
            #'wait-on-condition
            ready-p on-ready interval (1- max-attempts)))))

(defun wait-on-buffer-text (buffer match-regexp on-ready interval max-attempts)
  (let ((match-regexp match-regexp)
        (buffer buffer))
    (wait-on-condition (lambda () (string-match-p match-regexp
                                                  (save-excursion
                                                    (switch-to-buffer buffer)
                                                    (buffer-string))))
                       on-ready
                       interval
                       max-attempts)))

(defun run-cider-project (project-name
                          project-file-path
                          clj-file-path
                          cljs-file-path
                          clj-test-file-path
                          figwheel-port
                          cljs-user-ns
                          clj-repl-forms
                          cljs-repl-forms)
  (let ((project-name project-name)
        (project-file-path project-file-path)
        (clj-file-path clj-file-path)
        (cljs-file-path cljs-file-path)
        (clj-test-file-path clj-test-file-path)
        (figwheel-port figwheel-port)
        (cljs-user-ns cljs-user-ns)
        (clj-repl-forms clj-repl-forms)
        (cljs-repl-forms cljs-repl-forms))
    (cl-labels
        ((open-project
          ()
          (find-file project-file-path))
         (start-clj
          ()
          (save-excursion
            (find-file clj-file-path)
            (cider-connect
             `(:host
               "localhost"
               :port
               ,(cl-second (assoc project-name (cider-locate-running-nrepl-ports)))))))
         (start-cljs
          ()
          (save-excursion
            (find-file cljs-file-path)
            (cider-connect-figwheel)))
         (link-sesman-dirs
          ()
          (save-excursion
            (find-file clj-file-path)
            (when-let ((clj-ses (match-sesman-session
                                 (format ".*cider-repl.*%s.*" project-name)
                                 (format "%d" figwheel-port))))
              (sesman-link-with-directory nil clj-ses)))
          (save-excursion
            (find-file cljs-file-path)
            (when-let ((cljs-ses (match-sesman-session
                                  (format ".*cider-repl.*%s.*%d.*"
                                          project-name figwheel-port))))
              (sesman-link-with-directory nil cljs-ses)))
          (save-excursion
            (find-file clj-test-file-path)
            (when-let ((clj-test-ses (match-sesman-session
                                      (format ".*cider-repl.*%s.*" project-name)
                                      (format "%d" figwheel-port))))
              (sesman-link-with-directory nil clj-test-ses))
            (kill-buffer)))
         (find-clj-repl
          ()
          (cl-first (match-buffer-name
                     (format ".*cider-repl.*%s.*" project-name)
                     (format "%d" figwheel-port))))
         (find-cljs-repl
          ()
          (cl-first (match-buffer-name
                     (format ".*cider-repl.*%s.*%d.*"
                             project-name figwheel-port))))
         (have-repl-buffers
          ()
          (not (null (and (find-clj-repl) (find-cljs-repl)))))
         (show-repl-buffers
          ()
          (let ((clj-repl (find-clj-repl))
                (cljs-repl (find-cljs-repl)))
            (delete-other-windows)
            (when clj-repl
              (switch-to-buffer clj-repl))
            (when cljs-repl
              (if clj-repl
                  (switch-to-buffer-other-window cljs-repl)
                (switch-to-buffer cljs-repl)))
            (init-repl-buffers)))
         (init-repl-buffers
          ()
          (let ((clj-repl (find-clj-repl))
                (cljs-repl (find-cljs-repl)))
            (when clj-repl
              (wait-on-buffer-text
               clj-repl
               "user>"
               (lambda ()
                 (run-with-timer
                  0.25 nil
                  (lambda ()
                    (let ((ns (save-excursion
                                (find-file clj-file-path)
                                (cider-current-ns))))
                      (switch-to-buffer clj-repl)
                      (insert (format "(in-ns '%s)" ns))
                      (cider-repl-return))
                    (run-with-timer
                     0.5 nil
                     (lambda ()
                       (switch-to-buffer clj-repl)
                       (dolist (s clj-repl-forms)
                         (insert (format "%s" s))
                         (cider-repl-return)))))))
               0.1 300))
            (when cljs-repl
              (wait-on-buffer-text
               cljs-repl
               "cljs\.user>"
               (lambda ()
                 (run-with-timer
                  1.0 nil
                  (lambda ()
                    (let ((ns (save-excursion
                                (find-file cljs-file-path)
                                (cider-current-ns))))
                      (switch-to-buffer cljs-repl)
                      (insert (format "(in-ns '%s)" ns))
                      (cider-repl-return))
                    (run-with-timer
                     1.0 nil
                     (lambda ()
                       (switch-to-buffer cljs-repl)
                       (dolist (s cljs-repl-forms)
                         (insert (format "%s" s))
                         (cider-repl-return)))))))
               0.1 300)))))
      (stop-cider-all)
      (open-project)
      (start-clj)
      (start-cljs)
      (link-sesman-dirs)
      (wait-on-condition #'have-repl-buffers #'show-repl-buffers 0.1 200))))
