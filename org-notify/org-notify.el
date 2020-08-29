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

(defun org-notify--all-raw ()
  (->> (or (jeff/org-query)
           nil)
       (org-ql-select org-agenda-files )))

(defun org-notify--expired-raw ()
  (->> `(ts-active :from (->> (ts-now)
                               (ts-adjust 'day -10000))
                   :to   (ts-now))
       (org-ql-select org-agenda-files)))

(defun org-notify--map-extract (xs)
  (mapcar 'org-notify--extract xs))

(defun org-notify--all ()
  (org-notify--map-extract (org-notify--all-raw)))

(defun org-notify--expired ()
  (org-notify--map-extract (org-notify--expired-raw)))

''(defun org-notify--unique-headlines (regexp agenda)
    "Return unique headlines from the results of REGEXP in AGENDA."
    (let ((matches (-distinct (-flatten (s-match-strings-all regexp agenda)))))
      (--map (org-notify--strip-prefix it) matches)))

''(defun org-notify--get-headlines ()
    "Return the current org agenda as text only."
    (with-temp-buffer
      (let ((org-agenda-sticky nil)
            (org-agenda-buffer-tmp-name (buffer-name)))
        (ignore-errors (org-agenda-list 3))
        (org-notify--unique-headlines
         org-notify-headline-regexp
         (buffer-substring-no-properties (point-min) (point-max))))))

(defun org-notify--headline-complete? (x)
  "Return whether HEADLINE has been completed."
  (plist-get )
  (if (plist-get x :done))
  (not (null ))

  (--any? (s-starts-with? it headline) org-done-keywords-for-agenda))

(defun org-notify--filter-active (deadlines)
  "Remove any completed headings from the provided DEADLINES."
  (-remove 'org-notify--headline-complete? deadlines))

(defun org-notify--strip-states (deadlines)
  "Remove the todo states from DEADLINES."
  (--map (s-trim (s-chop-prefixes org-todo-keywords-for-agenda it)) deadlines))

(defun org-notify-check ()
  "Check for active, due deadlines and initiate notifications."
  (interactive)
  ;; avoid interrupting current command.
  (unless (minibufferp)
    (save-window-excursion
      (save-excursion
        (save-restriction
          (let ((active (org-notify--filter-active (org-notify--get-headlines))))
            (dolist (dl (org-notify--strip-states active))
              (alert dl :title org-notify-notification-title))))))
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
