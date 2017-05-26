;;; beginend-test.el --- Tests for mark support in beginend         -*- lexical-binding: t; -*-

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

(defun beginend--goto-begin-2 ()
  "Go to point 2 of current buffer with `beginend--double-tap-begin'."
  (beginend--double-tap-begin
   (goto-char 2)))

(defun beginend--goto-end-2 ()
  "Go to point 2 of current buffer with `beginend--double-tap-end'."
  (beginend--double-tap-end
   (goto-char 2)))

(describe "beginend"

  (before-each
    (spy-on 'message)) ;; disable "Mark activated" messages

  (describe "pushes mark"
    (describe "when going to beginning"
      (it "if point is not at beginning"
        (let ((marker-point 5))
          (assess-with-preserved-buffer-list
           (with-current-buffer (generate-new-buffer "mark")
             (insert "foo bar baz\n")
             (goto-char marker-point)
             (expect (mark) :to-be nil)
             (beginend--goto-begin-2)
             (expect (length mark-ring) :to-be 0)
             (expect (point) :to-be 2)
             (expect (mark) :to-be marker-point)))))

      (it "only once"
        (let ((marker-point 5))
          (assess-with-preserved-buffer-list
           (with-current-buffer (generate-new-buffer "mark")
             (insert "foo bar baz\n")
             (goto-char marker-point)
             (expect (length mark-ring) :to-be 0)
             (beginend--goto-begin-2)
             (expect (length mark-ring) :to-be 0)
             (beginend--goto-begin-2)
             (expect (length mark-ring) :to-be 0))))))

    (describe "when going to end"
      (it "if point is not at end"
        (let ((marker-point 5))
          (assess-with-preserved-buffer-list
           (with-current-buffer (generate-new-buffer "mark")
             (insert "foo bar baz\n")
             (goto-char marker-point)
             (expect (mark) :to-be nil)
             (beginend--goto-end-2)
             (expect (length mark-ring) :to-be 0)
             (expect (point) :to-be 2)
             (expect (mark) :to-be marker-point)))))

      (it "only once"
        (let ((marker-point 5))
          (assess-with-preserved-buffer-list
           (with-current-buffer (generate-new-buffer "mark")
             (insert "foo bar baz\n")
             (goto-char marker-point)
             (expect (length mark-ring) :to-be 0)
             (beginend--goto-end-2)
             (expect (length mark-ring) :to-be 0)
             (beginend--goto-end-2)
             (expect (length mark-ring) :to-be 0)))))))

  (describe "does not push mark"
    (describe "when going to beginning"
      (it "if point is already at beginning"
        (assess-with-preserved-buffer-list
         (with-current-buffer (generate-new-buffer "mark")
           (insert "foo bar baz\n")
           (goto-char 2)
           (expect (mark) :to-be nil)
           (beginend--goto-begin-2)
           (expect (mark) :to-be nil))))

      (it "if point is at point-min"
        (assess-with-preserved-buffer-list
         (with-current-buffer (generate-new-buffer "mark")
           (insert "foo bar baz\n")
           (goto-char (point-min))
           (expect (mark) :to-be nil)
           (beginend--goto-begin-2)
           (expect (mark) :to-be nil)))))

    (describe "when going to end"
      (it "if point is already at end"
        (assess-with-preserved-buffer-list
         (with-current-buffer (generate-new-buffer "mark")
           (insert "foo bar baz\n")
           (goto-char 2)
           (expect (mark) :to-be nil)
           (beginend--goto-end-2)
           (expect (mark) :to-be nil))))

      (it "if point is at point-max"
        (assess-with-preserved-buffer-list
         (with-current-buffer (generate-new-buffer "mark")
           (insert "foo bar baz\n")
           (goto-char (point-max))
           (expect (mark) :to-be nil)
           (beginend--goto-end-2)
           (expect (mark) :to-be nil)))))))

(provide 'beginend-marks-test)
;;; beginend-marks-test.el ends here
