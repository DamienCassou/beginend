;;; beginend-narrowing.el --- Tests beginend when narrowing is in effect         -*- lexical-binding: t; -*-

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

(load "test/test-helper")

(require 'assess)
(require 'buttercup)

(require 'beginend)

(define-error 'dont-call-me "This code should not have been called" 'error)

(defun beginend--goto-begin ()
  "Raise error."
  (beginend--double-tap-begin
   (signal 'dont-call-me t)))

(defun beginend--goto-end ()
  "Raise error."
  (beginend--double-tap-end
   (signal 'dont-call-me t)))

(describe "beginend, when narrowing is active,"

  (before-each
    (spy-on 'message)) ;; disable "Mark activated/Mark set" messages

  (it "does not call double-tap code when going to beginning"
    (with-temp-buffer
      (insert "foo bar baz\n")
      (narrow-to-region 3 5)
      (expect #'beginend--goto-begin :not :to-throw 'dont-call-me)))

  (it "does not call double-tap code when going to end"
    (with-temp-buffer
      (insert "foo bar baz\n")
      (narrow-to-region 3 5)
      (expect #'beginend--goto-end :not :to-throw 'dont-call-me)))

  (it "goes to point-min when going to beginning"
    (with-temp-buffer
      (insert "foo bar baz\n")
      (narrow-to-region 3 5)
      (beginend--goto-begin)
      (expect (point) :to-be (point-min))))

  (it "goes to point-max when going to end"
    (with-temp-buffer
      (insert "foo bar baz\n")
      (narrow-to-region 3 5)
      (beginend--goto-end)
      (expect (point) :to-be (point-max))))

  (it "marks position when going to beginning"
    (let ((marker-point 4))
      (with-temp-buffer
        (insert "foo bar baz\n")
        (narrow-to-region 3 5)
        (goto-char marker-point)
        (beginend--goto-begin)
        (expect (mark) :to-be marker-point))))

  (it "marks position when going to end"
    (let ((marker-point 4))
      (with-temp-buffer
        (insert "foo bar baz\n")
        (narrow-to-region 3 5)
        (goto-char marker-point)
        (beginend--goto-end)
        (expect (mark) :to-be marker-point))))

  (it "does not mark point-min when going to beginning"
    (with-temp-buffer
      (insert "foo bar baz\n")
      (narrow-to-region 3 5)
      (goto-char (point-min))
      (beginend--goto-begin)
      (expect (mark) :to-be nil)))

  (it "does not mark point-max when going to end"
    (with-temp-buffer
      (insert "foo bar baz\n")
      (narrow-to-region 3 5)
      (goto-char (point-max))
      (beginend--goto-end)
      (expect (mark) :to-be nil))))

(provide 'beginend-narrowing-test)
;;; beginend-narrowing-test.el ends here
