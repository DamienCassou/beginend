;;; beginend-test.el --- Tests for dired support in beginend         -*- lexical-binding: t; -*-

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

(require 'dired)
(require 'dired-x)

(defun beginend-create-dired-buffer ()
  "Create and return a buffer ressembling a dired one."
  (assess-with-filesystem '("dir1/" "dir2/")
    (dired default-directory)
    (current-buffer)))

(defun beginend-dired-test (&rest spec)
  "Test dired from SPEC.
SPEC is a list of symbols representing features to activate."
  (let* ((begin-p (seq-contains spec 'begin-p))
         (omit (seq-contains spec 'omit))
         (hide-details (seq-contains spec 'hide-details))
         (hide-info-lines (not (seq-contains spec 'keep-info-lines)))
         (dired-hide-details-hide-information-lines hide-info-lines)
         (dired-omit-verbose nil))
    (with-current-buffer (beginend-create-dired-buffer)
      (goto-char 2) ;; any position
      (when omit
        (dired-omit-mode))
      (when hide-details
        (dired-hide-details-mode))
      (if begin-p
          (progn
            (beginend-dired-mode-goto-beginning)
            (expect (looking-at "dir1") :to-be-truthy))
        (beginend-dired-mode-goto-end)
        (expect (looking-at "dir2") :to-be-truthy))
      (kill-buffer))))

(describe "beginend in a dired buffer"
  (describe "without dired-hide-details-mode"
    (describe "without dired-omit-mode"
      (it "offers a command to go to first file"
        (beginend-dired-test 'begin-p))

      (it "offers a command to go to last file"
        (beginend-dired-test)))

    (describe "with dired-omit-mode"
      (it "offers a command to go to first file"
        (beginend-dired-test 'begin-p 'omit))

      (it "offers a command to go to last file with dired-omit-mode"
        (beginend-dired-test 'omit))))

  (describe "with-dired-hide-details-mode (also hides information lines)"
    (describe "without dired-omit-mode"
      (it "offers a command to go to first file"
        (beginend-dired-test 'begin-p 'hide-details))

      (it "offers a command to go to last file with dired-omit-mode"
        (beginend-dired-test 'hide-details)))

    (describe "with dired-omit-mode"
      (it "offers a command to go to first file"
        (beginend-dired-test 'begin-p 'omit 'hide-details))

      (it "offers a command to go to last file with dired-omit-mode"
        (beginend-dired-test 'omit 'hide-details))))

  (describe "with-dired-hide-details-mode (does not hide information lines)"
    (describe "without dired-omit-mode"
      (it "offers a command to go to first file"
        (beginend-dired-test 'begin-p 'hide-details 'keep-info-lines))

      (it "offers a command to go to last file with dired-omit-mode"
        (beginend-dired-test 'hide-details 'keep-info-lines)))

    (describe "with dired-omit-mode"
      (it "offers a command to go to first file"
        (beginend-dired-test 'begin-p 'omit 'hide-details 'keep-info-lines))

      (it "offers a command to go to last file with dired-omit-mode"
        (beginend-dired-test 'omit 'hide-details 'keep-info-lines)))))

(provide 'beginend-dired-test)
;;; beginend-dired-test.el ends here
