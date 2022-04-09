;;; beginend-org-test.el --- Tests for org support in beginend         -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2022  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Version: 2.0.1
;; URL: https://github.com/DamienCassou/beginend
;; Package-requires: ((emacs "25.3"))

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

;; Tests for beginend in org-mode.

;;; Code:

(load "test/test-helper")

(require 'assess)
(require 'buttercup)

(require 'beginend)

(require 'org)

(require 'seq)

(describe "beginend"
  (describe "in org-mode"
    (it "goes back to position 0 if that's a heading"
      (with-temp-buffer
        (insert "* heading 1\nsome text\n")
        (setf (point) 15)
        (beginend-org-mode-goto-beginning)
        (expect (looking-at-p "\\* heading 1"))))
    (it "goes to second line if that's where the 1st heading is"
      (with-temp-buffer
        (insert "some initial text\n* heading 1\nsome text\n")
        (setf (point) (point-min))
        (beginend-org-mode-goto-beginning)
        (expect (looking-at-p "\\* heading 1"))))
    (it "goes to 1st heading if the buffer starts with properties"
      (with-temp-buffer
        (insert "#+TITLE: a\n\n* heading 1")
        (setf (point) (point-min))
        (beginend-org-mode-goto-beginning)
        (expect (looking-at-p "\\* heading 1"))))
    (it "goes to (point-min) if there is just one character"
      (with-temp-buffer
        (insert "a")
        (beginend-org-mode-goto-beginning)
        (expect (looking-at-p "a"))))
    (it "stays at (point-min) if buffer is empty"
      (with-temp-buffer
        (setf (point) (point-min))
        (beginend-org-mode-goto-beginning)
        (expect (point) :to-equal (point-min))))))

(provide 'beginend-org-test)
;;; beginend-org-test.el ends here

;; Local Variables:
;; nameless-current-name: "beginend-org-test"
;; End:
