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
(declare-function dired-next-line "dired")
(eval-when-compile
  (defvar message-mode-map)
  (defvar mu4e-view-mode-map)
  (defvar mu4e-compose-mode-map)
  (defvar dired-mode-map))

(defun beginend--defkey (map command-begin command-end)
  "Bind \[beginning-of-buffer] and \[end-of-buffer] in MAP to COMMAND-BEGIN and COMMAND-END."
  (define-key map (vector 'remap 'beginning-of-buffer) command-begin)
  (define-key map (vector 'remap 'end-of-buffer) command-end))

(defun beginend--goto-nonwhitespace ()
  "Move point backward after the first non-whitespace character."
  (re-search-backward "[^[:space:]]" nil t)
  (forward-char))

(defmacro beginend--double-tap-begin (&rest body)
  "Evaluate &BODY and goto real beginning if that did not change point."
  (let ((tempvar (make-symbol "old-position")))
    `(let ((,tempvar (point)))
       ,@body
       (when (equal ,tempvar (point))
         (call-interactively #'beginning-of-buffer)))))

(defmacro beginend--double-tap-end (&rest body)
  "Evaluate &BODY and goto real end if that did not change point."
  (let ((tempvar (make-symbol "old-position")))
    `(let ((,tempvar (point)))
       ,@body
       (when (equal ,tempvar (point))
         (call-interactively #'end-of-buffer)))))

;;;###autoload
(defun beginend-setup-all ()
  "Use beginend on all compatible modes.
For `dired', this activates `beginend-dired-mode'.
For messages, this activates `beginend-message-mode'."
  (add-hook 'dired-mode-hook #'beginend-dired-mode)
  (add-hook 'message-mode-hook #'beginend-message-mode)
  (add-hook 'mu4e-view-mode-hook #'beginend-message-mode)
  (add-hook 'mu4e-compose-mode-hook #'beginend-message-mode))



;;; Message mode

;;;###autoload
(defun beginend-message-goto-beginning ()
  "Go to the beginning of an email, after the headers."
  (interactive)
  (beginend--double-tap-begin
   (message-goto-body)))

;;;###autoload
(defun beginend-message-goto-end ()
  "Go to the end of an email, before the signature."
  (interactive)
  (beginend--double-tap-end
   (call-interactively #'end-of-buffer)
   (when (re-search-backward "^-- $" nil t)
     (beginend--goto-nonwhitespace))))

(defun beginend--message-mode-map ()
  "Return a keymap for beginend mode in a message."
  (let ((map (make-sparse-keymap)))
    (beginend--defkey map
                      #'beginend-message-goto-beginning
                      #'beginend-message-goto-end)
    map))

(define-minor-mode beginend-message-mode
  nil  ; default documentation
  nil  ; init-value
  "be" ; lighter
  (beginend--message-mode-map))



;;; Dired mode

;;;###autoload
(defun beginend-dired-goto-beginning ()
  "Go to the beginning of a dired's buffer first file, after `.' and `..'."
  (interactive)
  (beginend--double-tap-begin
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
     (dired-next-line move))))

;;;###autoload
(defun beginend-dired-goto-end ()
  "Go at the end of a dired's buffer last file, before the empty line."
  (interactive)
  (beginend--double-tap-end
   (goto-char (point-max))
   (beginend--goto-nonwhitespace)))

(defun beginend--dired-mode-map ()
  "Return a keymap for beginend mode in dired."
  (let ((map (make-sparse-keymap)))
    (beginend--defkey map
                      #'beginend-dired-goto-beginning
                      #'beginend-dired-goto-end)
    map))

(define-minor-mode beginend-dired-mode
  nil  ; documentation
  nil  ; init-value
  "be" ; lighter
  (beginend--dired-mode-map))

(provide 'beginend)

;;; beginend.el ends here
