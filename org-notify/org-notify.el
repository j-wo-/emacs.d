;;; org-notify.el --- org-mode nodifications via alert and libnotify

;; Copyright (C) 2020 Jeff Workman

;; Author: Jeff Workman <jeff.workman@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((s "1.10.0") (dash "2.11.0") (alert "1.2") (org-ql))
;; Keywords: org, org-mode, alert, notify, notifications, calendar
;; URL: https://github.com/jeffwk/org-notify

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functions to display system notifications for
;; any org-mode deadlines that are due in your agenda. To perform a
;; one-shot check call (org-notify-deadlines). To enable repeated
;; checking call (org-notify-enable) and to disable call
;; (org-notify-disable). You can set the checking interval by changing
;; the org--interval variable to the number of seconds you'd
;; like.


;;; Code:

(require 's)
(require 'dash)
(require 'alert)
(require 'org-agenda)
(require 'org-ql)

(defvar org-notify-interval 300
  "Interval in seconds to recheck and display schedule.")

(defvar org-notify-fade-time 10)

(defvar org-notify-notification-title "*org*"
  "Title to be sent with notify-send.")

(defmacro org-notify--query-all ()
  ;; `'(ts :on today)
  `'(ts-active :from (->> (ts-now)
                          (ts-adjust 'day -2))
               :to   (->> (ts-now)
                          (ts-adjust 'day 1))))

(defun org-notify--extract (x)
  (let* ((hl (car (alist-get 'headline (list x))))
         (hl-value (plist-get hl :raw-value))
         (scheduled (plist-get hl :scheduled))
         (scheduled-raw (-> scheduled
                            (plist-get 'timestamp)
                            (plist-get :raw-value)))
         (deadline (plist-get hl :deadline))
         (deadline-raw (-> deadline
                           (plist-get 'timestamp)
                           (plist-get :raw-value)))
         (todo-type (plist-get hl :todo-type))
         (todo-keyword (plist-get hl :todo-keyword))
         (ret (list)))
    (when hl-value
      (setq ret (plist-put ret :title hl-value)))
    (when scheduled-raw
      (setq ret (plist-put ret :scheduled scheduled-raw)))
    (when deadline-raw
      (setq ret (plist-put ret :deadline deadline-raw)))
    (when todo-type
      (setq ret (plist-put ret :todo-type todo-type)))
    (when todo-keyword
      (setq ret (plist-put ret :todo-keyword
                           (substring-no-properties todo-keyword))))
    ret))

(defun org-notify--map-extract (xs)
  (mapcar 'org-notify--extract xs))

(defun org-notify--select (q)
  (->> (org-ql-select org-agenda-files q)
       (org-notify--map-extract)))

(defun org-notify--ongoing ()
  (org-notify--select
   `(and (scheduled      :from ,(->> (ts-now)
                                     (ts-adjust 'day -1))
                         :to   ,(ts-now))
         (deadline       :from ,(ts-now)
                         :to   ,(->> (ts-now)
                                     (ts-adjust 'day 1)))
         (not (done)))))

(defun org-notify--upcoming ()
  (org-notify--select
   `(and (scheduled      :from ,(ts-now)
                         :to   ,(->> (ts-now)
                                     (ts-adjust 'hour 4))))))

(defun org-notify--future ()
  (org-notify--select
   `(and (scheduled      :from ,(->> (ts-now)
                                     (ts-adjust 'hour 4))))))

(defun org-notify--expired ()
  (org-notify--select
   `(and (ts-active      :to   ,(ts-now))
         (not (deadline  :from ,(->> (ts-now)))))))

(defun org-notify--all ()
  `((:ongoing    . ,(org-notify--ongoing))
    (:upcoming   . ,(org-notify--upcoming))
    (:future     . ,(org-notify--future))
    (:expired    . ,(org-notify--expired))))

(defun org-notify--format (x)
  (cl-flet ((f
             (field)
             (plist-get x field))
            (format-time
             (x)
             (or (-some->> x (ts-format "%l:%M %p")) "")))
    (let* ((title (f :title))
           (scheduled (-some-> (f :scheduled) (ts-parse)))
           (deadline (-some-> (f :deadline) (ts-parse)))
           (result ""))
      (when title
        (setq result (format "%s" title)))
      (when (or scheduled deadline)
        (setq result (format "[%s - %s]\n%s"
                             (format-time scheduled)
                             (format-time deadline)
                             result)))
      result)))

(defun org-notify-check ()
  "Check for active, due deadlines and initiate notifications."
  (interactive)
  ;; avoid interrupting current command.
  (unless (minibufferp)
    (let ((all (org-notify--all)))
      (dolist (type (reverse '(:ongoing
                               :upcoming
                               ;; :future
                               :expired)))
        (let ((type-name (capitalize (substring (symbol-name type) 1)))
              (entries (alist-get type all))
              (text ""))
          (dolist (x entries)
            (let ((x-text (org-notify--format x)))
              (unless (= 0 (length x-text))
                (setq text (format "%s%s\n\n" text x-text)))))
          (unless (= 0 (length text))
            (setq text (format "\n%s" text))
            (let ((alert-fade-time org-notify-fade-time))
              (alert text :title type-name))))))
    (when (get-buffer org-agenda-buffer-name)
      (ignore-errors
        (with-current-buffer org-agenda-buffer-name
          (org-agenda-redo t))))))

(defun org-notify-enable ()
  "Enable the notification timer.  Cancels existing timer if running."
  (interactive)
  (org-notify-disable)
  (run-at-time 0 org-notify-interval 'org-notify-check))

(defun org-notify-disable ()
  "Cancel the running notification timer."
  (interactive)
  (dolist (timer timer-list)
    (if (eq (elt timer 5) 'org-notify-check)
        (cancel-timer timer))))

(provide 'org-notify)
;;; org-notify.el ends here
