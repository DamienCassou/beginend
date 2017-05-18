;;; beginend.el --- Redefine M-< and M-> for some modes   -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2017 Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Version: 1.1.0
;; GIT: https://github.com/DamienCassou/beginend
;; Package-Requires: ((emacs "24.4"))
;; Created: 01 Jun 2015

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
;; - in dired mode, M-< (respectively M->) go to first (respectively last) file
;;   line
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

(defvar beginend--modes nil
  "List all beginend modes.
Each element has the form (std-mode-hook . beginend-mode) is the standard
mode hook (e.g., `dired-mode-hook') and beginend-mode is the beginend
mode (e.g., function `beginend-dired-mode') to activate with the hook.")

(defmacro beginend-define-mode (mode begin-body end-body)
  "Define a new beginend mode.
MODE is a name of an existing mode that should be adapted.

BEGIN-BODY and END-BODY are two `progn' expressions passed to respectively
`beginend--double-tap-begin' and `beginend--double-tap-end'."
  (declare (indent 1))
  (let* ((mode-name (symbol-name mode))
         (hook (intern (format "%s-hook" mode-name)))
         (beginfunc-name (intern (format "beginend-%s-goto-beginning" mode-name)))
         (endfunc-name (intern (format "beginend-%s-goto-end" mode-name)))
         (map-name (intern (format "beginend-%s-map" mode-name)))
         (beginend-mode-name (intern (format "beginend-%s" mode-name))))
    `(progn
       (defun ,beginfunc-name ()
         ,(format "Go to beginning of buffer in `%s'." mode-name)
         (interactive)
         (beginend--double-tap-begin
          ,@(cdr begin-body)))
       (defun ,endfunc-name ()
         ,(format "Go to beginning of buffer in `%s'." mode-name)
         (interactive)
         (beginend--double-tap-end
          ,@(cdr end-body)))
       (defvar ,map-name
         (let ((map (make-sparse-keymap)))
           (beginend--defkey map #',beginfunc-name #',endfunc-name)
           map)
         ,(format "Keymap for beginend in `%s'." mode-name))
       (define-minor-mode ,beginend-mode-name
         ,(format "Mode for beginend in `%s'.\n\\{%s}" mode-name map-name)
         :lighter " be"
         :keymap ,map-name)
       (add-to-list 'beginend--modes
                    (cons ',hook
                          #',beginend-mode-name)))))

;;; Modes

(beginend-define-mode message-mode
  (progn
    (message-goto-body))
  (progn
    (call-interactively #'end-of-buffer)
    (when (re-search-backward "^-- $" nil t)
      (beginend--goto-nonwhitespace))))

(beginend-define-mode dired-mode
  (progn
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
  (progn
    (goto-char (point-max))
    (beginend--goto-nonwhitespace)))

;;;###autoload
(defun beginend-setup-all ()
  "Use beginend on all compatible modes.
For `dired', this activates function `beginend-dired-mode'.
For messages, this activates function `beginend-message-mode'."
  (mapc (lambda (pair)
          (add-hook (car pair) (cdr pair)))
        beginend--modes)
  (add-hook 'mu4e-view-mode-hook #'beginend-message-mode)
  (add-hook 'mu4e-compose-mode-hook #'beginend-message-mode))


(provide 'beginend)

;;; beginend.el ends here

;;  LocalWords:  beginend
