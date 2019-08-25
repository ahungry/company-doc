;;; company-doc.el --- A better way to see documentation. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Matthew Carter <m@ahungry.com>

;; Author: Matthew Carter <m@ahungry.com>
;; Maintainer: Matthew Carter <m@ahungry.com>
;; URL: https://github.com/ahungry/company-doc
;; Version: 0.0.1
;; Keywords: ahungry convenience auto-complete company
;; Package-Requires: ((emacs "25.1") (company "0.0.1") (dash "0.0.1") (popup "0.0.1"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; An enhanced scrolling experience when quickly navigating through a buffer
;; with fast scrolling (usually via key-repeat instead of manual scrolling).

;;; Code:

(require 'company)
(require 'cl-lib)
(require 'popup)
(require 'dash)

(defvar-local company-doc--timer nil
  "Doc idle timer.")

(defvar company-doc-delay 0.5)

(defun company-doc--set-timer ()
  (when (null company-doc--timer)
    (setq company-doc--timer
          (run-with-idle-timer company-doc-delay nil
                               'company-doc--show))))

(defun company-doc--cancel-timer ()
  (when (timerp company-doc--timer)
    (cancel-timer company-doc--timer)
    (setq company-doc--timer nil)))

(defun company-doc--popper-kill ()
  "Kill the reply buffer entirely."
  (interactive)
  (company-doc--cancel-timer)
  (let ((window-to-delete (get-buffer-window "*doc-popper*")))
    (when window-to-delete
      (delete-window window-to-delete))))

(defun company-doc--popper-focus (buf &optional win)
  "Select the buffer BUF in the window WIN by splitting it.
If WIN is nil, the selected window is splitted."
  (let* ((win (or win (selected-window)))
         (size
          (let ((rest (- (window-height win) 15)))
            (if (<= rest 3)
                ;; To avoid an error due to a too small window.
                nil
              rest)))
         (new-win (split-window win size)))
    (select-window new-win)
    (switch-to-buffer buf)))

(defun company-doc--popper-show (s)
  "Pop up a buffer that we can easily pop-down / close."
  (interactive)
  (while-no-input
    (let ((reply-buffer (or (get-buffer "*doc-popper*")
                            (generate-new-buffer "*doc-popper*")))
          (this-win (selected-window)))
      (if (get-buffer-window "*doc-popper*" 'visible)
          (progn
            (set-buffer "*doc-popper*")
            (erase-buffer)
            (insert s))
        (progn
          (company-doc--popper-focus reply-buffer)
          (insert s)
          (add-text-properties (point-min) (point-max)
                               `(font-lock-face font-lock-comment-face rear-nonsticky t))
          (font-lock-flush)))
      (select-window this-win)))
  (format "%s" s))

;; (defun company-show-doc-buffer ()
;;   "Temporarily show the documentation buffer for the selection."
;;   (interactive)
;;   (let (other-window-scroll-buffer)
;;     (company--electric-do
;;       (let* ((selected (nth company-selection company-candidates))
;;              (doc-buffer (or (company-call-backend 'doc-buffer selected)
;;                              (user-error "No documentation available")))
;;              start)
;;         (when (consp doc-buffer)
;;           (setq start (cdr doc-buffer)
;;                 doc-buffer (car doc-buffer)))
;;         (setq other-window-scroll-buffer (get-buffer doc-buffer))
;;         (let ((win (display-buffer doc-buffer t)))
;;           (set-window-start win (if start start (point-min))))))))

(defun company-doc--get-doc-string-for-selection (selection)
  "Get doc string for whatever the SELECTION string should be."
  (let* ((doc-buffer (or (company-call-backend 'doc-buffer selection)
                         nil
                         ;; (user-error "No documentation available.")
                         ))
         start)
    (if doc-buffer
        (progn
          (when (consp doc-buffer)
            (setq start (cdr doc-buffer)
                  doc-buffer (car doc-buffer)))
          (with-current-buffer doc-buffer
            (save-restriction
              (widen)
              (buffer-substring-no-properties
               (if start start (point-min))
               (point-max)))))
      "")))

(defvar company-doc-memoization-table (list))

(defun company-doc--get-doc-string-for-selection-memoized (selection)
  "Just do the same but memoize it."
  (company-doc--cancel-timer)
  (let* ((cached (plist-get company-doc-memoization-table selection)))
    (if cached cached
      (let ((result (company-doc--get-doc-string-for-selection selection)))
        (setf company-doc-memoization-table
              (plist-put company-doc-memoization-table selection result))
        result))))

(defun company-doc--get-doc-string ()
  "Get doc string for whatever the selection string should be."
  (let* ((selection (nth company-selection company-candidates)))
    (company-doc--get-doc-string-for-selection-memoized selection)))

(defun company-doc--get-tty-point-x ()
  "Get the point to place cursor for our tty."
  (let ((points (- (window-end) (window-start))))
     (+ (window-start) (/ points 4))))

(defvar company-doc--point 0)

(defun company-doc--set-point ()
  "Set the point before we start moving around."
  (setq company-doc--point (point)))

(defun company-doc--get-tty-point ()
  "Get the point to place cursor for our tty."
  (+ company-doc--point 100))

(defun company-doc--show-tooltip (s)
  "Print a tooltip showing S for tty user."
  (while-no-input
    (when (stringp s)
      (with-no-warnings
        (popup-tip s
                   :point (company-doc--get-tty-point)
                   :around t
                   :scroll-bar t
                   :margin t))))
  (format "%s" s))

(defun company-doc--show ()
  "Run the main routine."
  (-> (company-doc--get-doc-string)
      company-doc--popper-show
      company-doc--show-tooltip))

(defun company-doc-frontend (command)
  "COMMAND is implemented according to the `company-mode' frontends interface."
  (cl-case command
    (pre-command (company-doc--set-point))
    ;; (pre-command (message "pre: %s" (car company-candidates)))
    ;; (post-command (message "post: %s" (car company-candidates)))
    (post-command (company-doc--set-timer))
    ;; (post-command (message (company-doc--get-doc-string)))
    ;; (post-command (company-doc--popper-show "Heh"))
    (hide (company-doc--popper-kill))
    ;; (hide (company-doc--popper-kill))
    ))

(defun company-doc-enable ()
  "Setup the mode."
  (add-to-list 'company-frontends #'company-doc-frontend :append))

(provide 'company-doc)
;;; company-doc.el ends here
