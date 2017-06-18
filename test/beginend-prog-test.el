;;; beginend-prog-test.el --- Tests for beginend in prog-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>

;; This program is free software; you can redistribute it and/or modify
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

;;

;;; Code:

(load "test/test-helper.el")

(require 'buttercup)

(require 'beginend)

(require 'font-lock)

(eval-when-compile
  (when (= emacs-major-version 24)
    (defun font-lock-ensure ()
      (font-lock-set-defaults)
      (font-lock-default-fontify-buffer))))

(describe "buttercup"
  (describe "in prog-mode"
    (it "uses prog-mode-code-position-p to know where code begins"
      (with-temp-buffer
        (insert "line1\nline2\nline3\nline4\nline5\nline6\nline7\n")
        (goto-char 2)
        ;; workaround for https://github.com/jorgenschaefer/emacs-buttercup/issues/84
        (cl-letf (((symbol-function 'beginend--prog-mode-code-position-p)
                   (lambda ()
                     (let ((line (line-number-at-pos)))
                       (and (>= line 3) (<= line 5))))))
          (beginend-prog-mode-goto-beginning))
        (expect (looking-at "line3") :to-be-truthy)))

    (it "uses prog-mode-code-position-p to know where code end"
      (with-temp-buffer
        (insert "line1\nline2\nline3\nline4\nline5\nline6\nline7\n")
        (goto-char 2)
        ;; workaround for https://github.com/jorgenschaefer/emacs-buttercup/issues/84
        (cl-letf (((symbol-function 'beginend--prog-mode-code-position-p)
                   (lambda ()
                     (let ((line (line-number-at-pos)))
                       (and (>= line 3) (<= line 5))))))
          (beginend-prog-mode-goto-end))
        (expect (line-number-at-pos) :to-be 5)))

    (it "moves point to end of last code line"
      (with-temp-buffer
        (insert "line1\nline2\nline3\nline4\nline5\nline6\nline7\n")
        (goto-char 2)
        ;; workaround for https://github.com/jorgenschaefer/emacs-buttercup/issues/84
        (cl-letf (((symbol-function 'beginend--prog-mode-code-position-p)
                   (lambda ()
                     (let ((line (line-number-at-pos)))
                       (and (>= line 3) (<= line 5))))))
          (beginend-prog-mode-goto-end))
        (expect (line-number-at-pos) :to-be 5)
        (expect (point) :to-be (line-end-position)))))

  (describe "beginend--prog-mode-code-position-p"
    (it "returns non-nil for a line of code"
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(defvar foo 3)\n")
        (goto-char (point-min))
        (expect (beginend--prog-mode-code-position-p) :to-be-truthy)))

    (describe "returns nil"
      (it "for a line of comment"
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert ";; (defvar foo 3)\n")
          (font-lock-ensure)
          (goto-char (point-min))
          (expect (beginend--prog-mode-code-position-p) :to-be nil)))

      (it "for an empty line"
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert "\n")
          (font-lock-ensure)
          (goto-char (point-min))
          (expect (beginend--prog-mode-code-position-p) :to-be nil)))

      (it "for a line with ^L"
        (with-temp-buffer
          (emacs-lisp-mode)
          (insert ?\f ?\n)
          (font-lock-ensure)
          (goto-char (point-min))
          (expect (beginend--prog-mode-code-position-p) :to-be nil))))))

(provide 'beginend-prog-test)
;;; beginend-prog-test.el ends here
