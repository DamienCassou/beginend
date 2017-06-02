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
  "Go to point 2."
  (beginend--double-tap-begin
   (goto-char 2)))

(defun beginend--goto-end ()
  "Go to point 2."
  (beginend--double-tap-end
   (goto-char 2)))

(describe "beginend, when narrowing is active,"

  (before-each
    (spy-on 'message)) ;; disable "Mark activated/Mark set" messages

  (it "goes to point-min if beginning is outside the narrowed region"
    (with-temp-buffer
      (insert "foo bar baz\n")
      (narrow-to-region 3 5)
      (beginend--goto-begin)
      (expect (point) :to-be (point-min))))

  (it "goes to beginning if beginning is inside the narrowed region"
    (with-temp-buffer
      (insert "foo bar baz\n")
      (narrow-to-region 1 3)
      (beginend--goto-begin)
      (expect (point) :to-be 2)))

  (it "goes to point-max if end is outside the narrowed region"
    (with-temp-buffer
      (insert "foo bar baz\n")
      (narrow-to-region 3 5)
      (beginend--goto-end)
      (expect (point) :to-be (point-max))))

  (it "goes to end if end is inside the narrowed region"
    (with-temp-buffer
      (insert "foo bar baz\n")
      (narrow-to-region 1 3)
      (beginend--goto-end)
      (expect (point) :to-be 2)))

  (describe "does not move point"
    (it "when going to beginning and beginning outside the narrowed region if point is already at point-min"
      (with-temp-buffer
        (insert "foo bar baz\n")
        (narrow-to-region 3 5)
        (goto-char (point-min))
        (beginend--goto-begin)
        (expect (point) :to-be (point-min))))

    (it "when going to end and end outside the narrowed region if point is already at point-max"
      (with-temp-buffer
        (insert "foo bar baz\n")
        (narrow-to-region 3 5)
        (goto-char (point-max))
        (beginend--goto-end)
        (expect (point) :to-be (point-max)))))

  (describe "does not mark"
    (it "when going to beginning and beginning outside the narrowed region if point is already at point-min"
      (with-temp-buffer
        (insert "foo bar baz\n")
        (narrow-to-region 3 5)
        (goto-char (point-min))
        (beginend--goto-begin)
        (expect (mark) :to-be nil)))

    (it "when going to end and end outside the narrowed region if point is already at point-max"
      (with-temp-buffer
        (insert "foo bar baz\n")
        (narrow-to-region 3 5)
        (goto-char (point-max))
        (beginend--goto-end)
        (expect (mark) :to-be nil))))

  (describe "correctly detects out of bounds"
    (it "when point is before point-min"
      (with-temp-buffer
        (insert "123")
        (expect (beginend--out-of-bounds-p 0) :to-be t)))

    (it "when point is after point-max"
      (with-temp-buffer
        (insert "123")
        (expect (beginend--out-of-bounds-p 5) :to-be t)))

    (it "when point is between point-min point-max"
      (with-temp-buffer
        (insert "123")
        (expect (beginend--out-of-bounds-p 2) :to-be nil)))))

(provide 'beginend-narrowing-test)
;;; beginend-narrowing-test.el ends here
