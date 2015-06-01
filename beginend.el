;;; beginend.el --- Redefine M-< and M-> for some modes   -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Version: 0.1

;; This file is not part of GNU Emacs.

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

;; Redefine M-< and M-> for some modes

;;; Code:


;;; Helper code

;; Avoid warnings about undefined variables and functions
(declare-function message-goto-body "message")
(eval-when-compile
  (defvar message-mode-map)
  (defvar mu4e-view-mode-map)
  (defvar dired-mode-map))

(defun beginend--defkey (map command-begin command-end)
  "Bind \[beginning-of-buffer] and \[end-of-buffer] in MAP to COMMAND-BEGIN and COMMAND-END."
  (define-key map (vector 'remap 'beginning-of-buffer) command-begin)
  (define-key map (vector 'remap 'end-of-buffer) command-end))




;;; Message mode

;;;###autoload
(defun beginend-message-goto-beginning ()
  "Go to the beginning of an email, after the headers."
  (interactive)
  (let ((old-position (point)))
    (message-goto-body)
    (when (equal (point) old-position)
      (call-interactively #'beginning-of-buffer))))

(defun beginend-message-goto-end ()
  "Go to the end of an email, before the signature."
  (interactive)
  (let ((old-position (point))
        (message-position (save-excursion (message-goto-body) (point))))
    (call-interactively #'end-of-buffer)
    (when (re-search-backward "^-- $" message-position t)
      (call-interactively #'previous-line))
    (when (equal (point) old-position)
      (call-interactively #'end-of-buffer))))

(with-eval-after-load "message"
  (beginend--defkey message-mode-map
                    #'beginend-message-goto-beginning
                    #'beginend-message-goto-end)

  (with-eval-after-load "mu4e-view"
    (beginend--defkey mu4e-view-mode-map
                      #'beginend-message-goto-beginning
                      #'beginend-message-goto-end)))



;;; Dired mode

(defun beginend-dired-goto-beginning ()
  "Go to the beginning of a dired buffer, after `.' and `..'."
  (interactive)
  (goto-char (point-min))
  (let ((move 1))
    (when (and (boundp 'dired-omit-mode) dired-omit-mode)
      ;; dired-omit-mode hides `.' and `..'.
      (setf move (+ 2 move)))
    (when (and (boundp 'dired-hide-details-hide-information-lines)
               dired-hide-details-hide-information-lines)
      (setf move (1- move)))
    (dired-next-line (if (and (boundp 'dired-omit-mode)
                              dired-omit-mode)
                         1
                       4))))

(defun beginend-dired-goto-end ()
  "Go to the end of a dired buffer, before the empty line."
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(with-eval-after-load "dired"
  (beginend--defkey dired-mode-map
                    #'beginend-dired-goto-beginning
                    #'beginend-dired-goto-end))

(provide 'beginend)

;;; beginend.el ends here
