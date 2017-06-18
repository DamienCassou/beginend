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
    (before-each
      (spy-on #'beginend--prog-mode-code-position-p
              :and-call-fake (lambda ()
                               (<= 3 (line-number-at-pos) 5))))

    (it "uses prog-mode-code-position-p to know where code begins"
      (with-temp-buffer
        (insert "line1\nline2\nline3\nline4\nline5\nline6\nline7\n")
        (goto-char 2)
        (beginend-prog-mode-goto-beginning)
        (expect #'beginend--prog-mode-code-position-p :to-have-been-called)))

    (it "uses prog-mode-code-position-p to know where code end"
      (with-temp-buffer
        (insert "line1\nline2\nline3\nline4\nline5\nline6\nline7\n")
        (goto-char 2)
        (beginend-prog-mode-goto-end)
        (expect #'beginend--prog-mode-code-position-p :to-have-been-called)))

    (it "moves point to beginning of first code line"
      (with-temp-buffer
        (insert "line1\nline2\nline3\nline4\nline5\nline6\nline7\n")
        (goto-char 2)
        ;; workaround for https://github.com/jorgenschaefer/emacs-buttercup/issues/84
        (beginend-prog-mode-goto-beginning)
        (expect (line-number-at-pos) :to-be 3)
        (expect (point) :to-be (line-beginning-position))))

    (it "moves point to end of last code line"
      (with-temp-buffer
        (insert "line1\nline2\nline3\nline4\nline5\nline6\nline7\n")
        (goto-char 2)
        ;; workaround for https://github.com/jorgenschaefer/emacs-buttercup/issues/84
        (beginend-prog-mode-goto-end)
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
