;;; beginend.el --- Redefine M-< and M-> for some modes   -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Version: 0.1
;; GIT: https://github.com/DamienCassou/beginend
;; Package-Requires: ((emacs "24.4"))
;; Created: 01 Jun 2015
;; Keywords: dired message mu4e begin end beginning buffer

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
;;
;; - in dired mode, M-< (resp. M->) go to first (resp. last) file line
;;
;; - in message mode,
;;    - M-< go to first line of message body (after headings)
;;    - M-> go to last line before message signature
;;
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
  (message-goto-body))

(defun beginend-message-goto-end ()
  "Go to the end of an email, before the signature."
  (interactive)
  (call-interactively #'end-of-buffer)
  (when (re-search-backward "^-- $" nil t)
    (re-search-backward "[^[:space:]]" nil t)
    (forward-char)))

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
  (let ((move 4))
    (when (and (boundp 'dired-omit-mode) dired-omit-mode)
      ;; dired-omit-mode hides `.' and `..'.
      (setf move (- move 2)))
    (when (and (boundp 'dired-hide-details-hide-information-lines)
               dired-hide-details-hide-information-lines
               (boundp 'dired-hide-details-mode)
               dired-hide-details-mode)
      ;; 1 line containing directory size
      (setf move (- move 1)))
    (dired-next-line move)))

(defun beginend-dired-goto-end ()
  "Go to the end of a dired buffer, before the empty line."
  (interactive)
  (goto-char (point-max))
  (re-search-backward "[^[:space:]]" nil t)
  (forward-char))

(with-eval-after-load "dired"
  (beginend--defkey dired-mode-map
                    #'beginend-dired-goto-beginning
                    #'beginend-dired-goto-end))

(provide 'beginend)

;;; beginend.el ends here
