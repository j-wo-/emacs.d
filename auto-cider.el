;;; -*- lexical-binding: t -*-

(require 'cl-lib)

(eval-when-compile
  (require 'dash)
  (require 'sesman)
  (require 'cider))

(defun all-sesman-sessions ()
  (sesman-sessions (sesman--system) t))

(defun test-buffer-name (buf regexp &optional exclude-regexp)
  (and (string-match regexp (buffer-name buf))
       (if (null exclude-regexp) t
         (not (string-match exclude-regexp (buffer-name buf))))))

(defun match-buffer-name (regexp &optional exclude-regexp)
  (--filter (test-buffer-name it regexp exclude-regexp)
            (buffer-list)))

(defun match-sesman-session (regexp &optional exclude-regexp)
  (->> (all-sesman-sessions)
       (--remove (not (test-buffer-name (cl-second it) regexp exclude-regexp)))
       (cl-first)))

(defun stop-cider-all ()
  (interactive)
  (dolist (buf (match-buffer-name "\*cider-repl\ .*"))
    (save-excursion
      (switch-to-buffer buf)
      (cider-quit))))

(defmacro with-delay (seconds &rest body)
  (declare (indent 1))
  `(let ((seconds ,seconds))
     (if (and (numberp seconds) (> seconds 0))
         (run-with-timer seconds nil (lambda () ,@body))
       (progn ,@body))))

(defvar --wait-on-condition-lock nil)

(defun --wait-on-condition (ready-p on-ready &optional interval timeout delay)
  (let* ((interval (or interval 0.025))
         (timeout (or timeout 5))
         (max-attempts (round (/ timeout interval)))
         (delay delay))
    (cond ((< max-attempts 1)
           nil)
          ((and (null --wait-on-condition-lock)
                (let (;; (--wait-on-condition-lock t)
                      )
                  (funcall ready-p)))
           (with-delay delay
             (let ((--wait-on-condition-lock t))
               (funcall on-ready))))
          (t (run-with-timer interval nil
                             #'--wait-on-condition
                             ready-p on-ready interval (- timeout interval) delay)))))

(defmacro wait-on-condition (test-form args &rest body)
  (declare (indent 2))
  `(--wait-on-condition (lambda () ,test-form)
                        (lambda () ,@body)
                        ,@args))
;;(wait-on-condition t nil (alert "hi"))

(defun --wait-on-buffer-text (buffer match-regexp on-ready
                                     &optional interval timeout delay)
  (let ((buffer buffer)
        (match-regexp match-regexp)
        ;; (delay (or delay 0.02))
        )
    (--wait-on-condition (lambda ()
                           (string-match-p match-regexp
                                           (with-current-buffer buffer
                                             (buffer-string))))
                         on-ready interval timeout delay)))

(defmacro wait-on-buffer-text (buffer match-regexp args &rest body)
  (declare (indent 3))
  `(let ((buffer ,buffer)
         (match-regexp ,match-regexp))
     (--wait-on-buffer-text buffer match-regexp
                            (lambda () ,@body)
                            ,@args)))
;;(wait-on-buffer-text (current-buffer) "body\) *$" (0.1 3) (alert "hi"))

(defvar --run-cider-progress nil)
(defvar --run-cider-target nil)

(defun --init-run-cider (target)
  (setq --run-cider-progress 0
        --run-cider-target target))

(defun --update-run-cider-progress ()
  (setq --run-cider-progress (+ 1 --run-cider-progress)))

(defun --run-cider-finished ()
  (= --run-cider-progress --run-cider-target))

(defun run-cider-project (project-name
                          project-file-path
                          clj-file-path
                          cljs-file-path
                          clj-test-file-path
                          figwheel-port
                          cljs-user-ns
                          clj-repl-forms
                          cljs-repl-forms)
  (--init-run-cider 2)
  (let ((start-time (current-time))
        (project-name project-name)
        (project-file-path project-file-path)
        (clj-file-path clj-file-path)
        (cljs-file-path cljs-file-path)
        (clj-test-file-path clj-test-file-path)
        (figwheel-port figwheel-port)
        ;; (cljs-user-ns cljs-user-ns)
        (clj-repl-forms clj-repl-forms)
        (cljs-repl-forms cljs-repl-forms)
        (clj-file-buffer nil)
        (cljs-file-buffer nil))
    (cl-labels
        ((open-project
          ()
          (find-file project-file-path))
         (start-clj
          ()
          (find-file clj-file-path)
          (setq clj-file-buffer (current-buffer))
          (cider-connect
           `(:host
             "localhost"
             :port
             ,(cl-second (assoc project-name (cider-locate-running-nrepl-ports))))))
         (start-cljs
          ()
          (find-file cljs-file-path)
          (setq cljs-file-buffer (current-buffer))
          (cider-connect-cljs
           `(:host "localhost" :port ,figwheel-port :cljs-repl-type shadow-select)))
         (link-sesman-dirs
          ()
          (with-current-buffer clj-file-buffer
            (when-let ((clj-ses (match-sesman-session
                                 (format ".*cider-repl.*%s.*" project-name)
                                 (format "%d" figwheel-port))))
              (sesman-link-with-directory nil clj-ses)))
          (with-current-buffer cljs-file-buffer
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
          (&optional no-init)
          (let ((clj-repl (find-clj-repl))
                (cljs-repl (find-cljs-repl)))
            (delete-other-windows)
            (when clj-repl
              (switch-to-buffer clj-repl)
              (when no-init
                (goto-char (point-max))))
            (when cljs-repl
              (if clj-repl
                  (switch-to-buffer-other-window cljs-repl)
                (switch-to-buffer cljs-repl))
              (when no-init
                (goto-char (point-max))))
            (unless no-init
              (init-repl-buffers))))
         (init-repl-buffers
          ()
          (let ((clj-repl (find-clj-repl))
                (cljs-repl (find-cljs-repl)))
            (when clj-repl
              (wait-on-buffer-text clj-repl "user>" ()
                (let ((ns (with-current-buffer clj-file-buffer
                            (cider-current-ns))))
                  (save-excursion
                    (switch-to-buffer clj-repl)
                    (insert (format "(in-ns '%s)" ns))
                    (cider-repl-return))
                  (wait-on-buffer-text clj-repl (format "%s> *$" ns) ()
                    (dolist (s clj-repl-forms)
                      (save-excursion
                        (switch-to-buffer clj-repl)
                        (insert (format "%s" s))
                        (cider-repl-return)))
                    (--update-run-cider-progress)))))
            (when cljs-repl
              (wait-on-buffer-text cljs-repl "cljs\.user>" (nil nil 0.025)
                (let ((ns (with-current-buffer cljs-file-buffer
                            (cider-current-ns))))
                  (save-excursion
                    (switch-to-buffer cljs-repl)
                    (insert (format "(in-ns '%s)" ns))
                    (cider-repl-return))
                  (wait-on-buffer-text cljs-repl (format "%s> *$" ns) (nil nil 0.05)
                    (save-excursion
                      (switch-to-buffer cljs-file-buffer)
                      (cider-load-buffer-reload-repl))
                    (dolist (s cljs-repl-forms)
                      (save-excursion
                        (switch-to-buffer cljs-repl)
                        (goto-char (point-max))
                        (insert (format "%s" s))
                        (cider-repl-return)))
                    (--update-run-cider-progress)))))
            (wait-on-condition (--run-cider-finished) ()
              (show-repl-buffers t)
              (--elapsed-alert project-name (--elapsed-seconds start-time))))))
      (stop-cider-all)
      (open-project)
      (start-clj)
      (start-cljs)
      (link-sesman-dirs)
      (wait-on-condition (have-repl-buffers) ()
        (show-repl-buffers)))))

;;(load-local "auto-cider")
