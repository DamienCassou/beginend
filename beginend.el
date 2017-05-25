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

(defun beginend--defkey (map command-begin command-end)
  "Bind \[beginning-of-buffer] and \[end-of-buffer] in MAP to COMMAND-BEGIN and COMMAND-END."
  (define-key map (vector 'remap 'beginning-of-buffer) command-begin)
  (define-key map (vector 'remap 'end-of-buffer) command-end))

(defun beginend--goto-nonwhitespace ()
  "Move point backward after the first non-whitespace character."
  (re-search-backward "[^[:space:]]" nil t)
  (forward-char))

(defmacro beginend--double-tap (extremum &rest body)
  "Go to point EXTREMUM if executing BODY did not change point."
  (declare (debug (form body))
           (indent 1))
  (let ((oldpos-var (make-symbol "old-position"))
        (extremum-var (make-symbol "extremum")))
    `(let ((,oldpos-var (point))
           (,extremum-var ,extremum))
       (goto-char ,extremum-var)
       ,@body
       (when (= ,oldpos-var (point))
         (goto-char ,extremum-var)))))

(defmacro beginend--double-tap-begin (&rest body)
  "Evaluate BODY and go-to `point-min' if point did not move."
  (declare (debug (body)))
  `(beginend--double-tap (point-min)
     ,@body))

(defmacro beginend--double-tap-end (&rest body)
  "Evaluate BODY and go-to `point-max' if point did not move."
  (declare (debug (body)))
  `(beginend--double-tap (point-max)
     ,@body))

(defvar beginend-modes
  '(
    (mu4e-view-mode-hook . beginend-message-mode)
    (mu4e-compose-mode-hook . beginend-message-mode)
    )
  "List all beginend modes.
Each element has the form (STD-MODE-HOOK . BEGINEND-MODE).  STD-MODE-HOOK
is the standard mode hook (e.g., `dired-mode-hook') to which
BEGINEND-MODE (e.g., function `beginend-dired-mode') should be added.")

(defmacro beginend-define-mode (mode begin-body end-body)
  "Define a new beginend mode.
MODE is a name of an existing mode that should be adapted.

BEGIN-BODY and END-BODY are two `progn' expressions passed to respectively
`beginend--double-tap-begin' and `beginend--double-tap-end'."
  (declare (indent 1)
           (debug (symbolp ("progn" body) ("progn" body))))
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
       (add-to-list 'beginend-modes
                    (cons ',hook
                          #',beginend-mode-name)))))


;;; Modes

(declare-function message-goto-body "message")

(beginend-define-mode message-mode
  (progn
    (message-goto-body))
  (progn
    (when (re-search-backward "^-- $" nil t)
      (beginend--goto-nonwhitespace))))

(declare-function dired-next-line "dired")

(beginend-define-mode dired-mode
  (progn
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
    (beginend--goto-nonwhitespace)))

(beginend-define-mode occur-mode
  (progn
    (occur-next 1))
  (progn
    (occur-prev 1)))

(declare-function ibuffer-forward-line "ibuffer")
(declare-function ibuffer-backward-line "ibuffer")

(beginend-define-mode ibuffer-mode
  (progn
    (ibuffer-forward-line 1))
  (progn
    (ibuffer-backward-line 1)))

(declare-function vc-dir-next-line "vc-dir")
(declare-function vc-dir-previous-line "vc-dir")

(beginend-define-mode vc-dir-mode
  (progn
    (vc-dir-next-line 1))
  (progn
    (vc-dir-previous-line 1)))


(declare-function bs-up "bs")
(declare-function bs-down "bs")

(beginend-define-mode bs-mode
  (progn
    (bs-down 2))
  (progn
    (bs-up 1)
    (bs-down 1)))

(beginend-define-mode recentf-dialog-mode
  (progn
    (when (re-search-forward "^  \\[" nil t)
      (goto-char (match-beginning 0))))
  (progn
    (re-search-backward "^  \\[" nil t)))

(declare-function org-agenda-next-item "org-agenda")
(declare-function org-agenda-previous-item "org-agenda")

(beginend-define-mode org-agenda-mode
  (progn
    (org-agenda-next-item 1))
  (progn
    (org-agenda-previous-item 1)))

(declare-function compilation-next-error "compile")
(declare-function compilation-previous-error "compile")

(beginend-define-mode compilation-mode
  (progn
    (compilation-next-error 1))
  (progn
    (compilation-previous-error 1)))

(declare-function notmuch-search-first-thread "notmuch")
(declare-function notmuch-search-last-thread "notmuch")

(beginend-define-mode notmuch-search-mode
  (progn
    (notmuch-search-first-thread)
    (beginning-of-line))
  (progn
    (notmuch-search-last-thread)
    (end-of-line)))

(beginend-define-mode elfeed-search-mode
  (progn)
  (progn
    (forward-line -2)))

(declare-function prodigy-first "prodigy")
(declare-function prodigy-last "prodigy")

(beginend-define-mode prodigy-mode
  (progn
    (prodigy-first))
  (progn
    (prodigy-last)))

;;;###autoload
(defun beginend-setup-all ()
  "Use beginend on all compatible modes.
For example, this activates function `beginend-dired-mode' in `dired' and
function `beginend-message-mode' in `message-mode'.  All affected minor
modes are described in `beginend-modes'."
  (mapc (lambda (pair)
          (add-hook (car pair) (cdr pair)))
        beginend-modes))

;;;###autoload
(defun beginend-unsetup-all ()
  "Remove beginend from all compatible modes in `beginend-modes'."
  (mapc (lambda (pair)
          (remove-hook (car pair) (cdr pair)))
        beginend-modes))

(define-minor-mode beginend-global-mode
  "Toggle beginend mode.
Interactively with no argument, this command toggles the mode.  A positive
prefix argument enables the mode, any other prefix argument disables it.
From Lisp, argument omitted or nil enables the mode, `toggle' toggles the
state.

When beginend mode is enabled, modes such as `dired-mode', `message-mode'
and `compilation-mode' will have their \\[beginning-of-buffer] and
\\[end-of-buffer] keys adapted to go to meaningful places."
  :lighter " be"
  :global t
  (if beginend-global-mode
      (beginend-setup-all)
    (beginend-unsetup-all)))


(provide 'beginend)

;;; beginend.el ends here

;;  LocalWords:  beginend
