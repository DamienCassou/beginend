;;; beginend-dired-test.el --- Tests for dired support in beginend         -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2023  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Version: 2.4.0
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

;; Tests for beginend in dired-mode.

;;; Code:

(load "test/test-helper")

(require 'assess)
(require 'buttercup)

(require 'beginend)

(require 'dired)
(require 'dired-x)

(require 'seq)

(defun beginend-dired-test--create-buffer (entries)
  "Create and return a buffer resembling a dired one.

ENTRIES is the file list.  The order is important.  ENTRIES are listed in that
order."
  (let ((spec (seq-filter (lambda (x) (not (member x '("." "..")))) entries)))
    (assess-with-filesystem spec
      (dired (cons default-directory entries))
      (current-buffer))))

(defun beginend-dired-test (fn location)
  "Execute FN at `point-min' and check that point is at the LOCATION.
This function moves point to `point-min' and executes FN with no parameter.
Then checks whether the point is at the LOCATION.
If LOCATION is a string, checks that point is at the beginning of the filename.
If LOCATION is a integer, checks that point is at this line.
If LOCATION is `bob', checks that point is at `point-min'.
If LOCATION is `eob', checks that point is at `point-max'.
Other values are invalid.  In this case, this function signals an error."
  (goto-char (point-min))
  (funcall fn)
  (cond ((stringp location)
         (expect (looking-at location) :to-be-truthy))
        ((integerp location)
         (expect location :to-equal (line-number-at-pos)))
        ((eq location 'bob)
         (expect (bobp) :to-be-truthy))
        ((eq location 'eob)
         (expect (eobp) :to-be-truthy))
        (t
         (error "Invalid test value: %s" location))))

(defun beginend-dired-test-begin (location)
  "Move the point to the first file line of current buffer.
Then checks whether the point is at the LOCATION.
If LOCATION is a string, checks that point is at the beginning of the filename.
If LOCATION is a integer, checks that point is at this line.
If LOCATION is `bob', checks that point is at `point-min'.
If LOCATION is `eob', checks that point is at `point-max'.
Other value are invalid.  In this case, this function signals an error."
  (beginend-dired-test 'beginend-dired-mode-goto-beginning location))

(defun beginend-dired-test-end (location)
  "Move the point to the last file line of current buffer.
Then checks whether the point is at the position.
If LOCATION is a string, checks that point is at the beginning of the filename.
If LOCATION is a integer, checks that point is at this line.
If LOCATION is `bob', checks that point is at `point-min'.
If LOCATION is `eob', checks that point is at `point-max'.
Other value are invalid.  In this case, this function signals an error."
  (beginend-dired-test 'beginend-dired-mode-goto-end location))

(defmacro beginend-dired-test--with-buffer (entries &rest body)
  "Create a temporary directory with ENTRIES and execute BODY.

ENTRIES is a list of all filenames to display, in the same order, in the dired
buffer.

Execute the forms in BODY within the dired buffer of the directory."
  (declare (indent 1))
  `(with-current-buffer (beginend-dired-test--create-buffer ,entries)
     ,@body))

(describe "beginend in a dired buffer"
  (it "ignores . and .. at the beginning"
    (beginend-dired-test--with-buffer '("." ".." "dir1" "dir2")
      (beginend-dired-test-begin "dir1")
      (beginend-dired-test-end "dir2")))

  (it "ignores . and .. at the end"
    (beginend-dired-test--with-buffer '("dir1" "dir2" "." "..")
      (beginend-dired-test-begin "dir1")
      (beginend-dired-test-end "dir2")))

  (it "ignores . at the beginning and .. at the end"
    (beginend-dired-test--with-buffer '("." "dir1" "dir2" "..")
      (beginend-dired-test-begin "dir1")
      (beginend-dired-test-end "dir2")))

  (it "ignores . and .. in the middle"
    (beginend-dired-test--with-buffer '("dir1" "." ".." "dir2")
      (beginend-dired-test-begin "dir1")
      (beginend-dired-test-end "dir2")))

  (it "ignores . and .. when they are hidden"
    (beginend-dired-test--with-buffer '("dir1" "dir2")
      (beginend-dired-test-begin "dir1")
      (beginend-dired-test-end "dir2")))

  (it "ignores . and .. when they are not hidden"
    (beginend-dired-test--with-buffer '(".." "dir2" "." "dir1")
      (beginend-dired-test-begin 3)
      (beginend-dired-test-end 5)))

  (it "ignores . and .. when they are hidden and the directory is empty"
    (beginend-dired-test--with-buffer '()
      (beginend-dired-test-begin 'bob)
      (beginend-dired-test-end 'eob)))

  (it "ignores . and .. when they are not hidden and the directory is empty"
    (beginend-dired-test--with-buffer '("." "..")
      (beginend-dired-test-begin 'bob)
      (beginend-dired-test-end 'eob))))

(provide 'beginend-dired-test)
;;; beginend-dired-test.el ends here

;; Local Variables:
;; nameless-current-name: "beginend-dired-test"
;; End:
