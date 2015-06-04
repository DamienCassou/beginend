;;; beginend-steps.el --- Step definitions for beginend
;;; Commentary:
;;
;; This file contains your project specific step definitions. All files in this
;; directory whose names end with "-steps.el" will be loaded automatically by
;; Ecukes.
;;
;;; Code:

(require 'f)
(require 'thingatpt)

;; defined in env.el (which is loaded automatically by ecukes)
(eval-when-compile (defvar beginend--test-tempdir))

(Given "^I setup dired$"
       (lambda ()
         (dired beginend--test-tempdir)
         ))

(Given "^I setup a message$"
       (lambda ()
         (let ((buffer (generate-new-buffer "*beginend*")))
           (switch-to-buffer buffer)
           (insert "From: someaddresse@foo.com\n\n")
           (insert "Hello,\n\nBye,\n-- \nSome signature\n")
           (message-mode))))

(Given "^I activate dired-omit-mode$"
       (lambda ()
         (require 'dired-x)
         (dired-omit-mode)
         ))

(Given "^I deactivate dired-omit-mode$"
       (lambda ()
         (require 'dired-x)
         (dired-omit-mode -1)
         ))

(And "^I deactivate dired-hide-details-mode$"
     (lambda ()
       (dired-hide-details-mode -1)))

(And "^I activate dired-hide-details-mode$"
     (lambda ()
       (dired-hide-details-mode)))

(Given "^I activate beginend-message-mode$"
       (lambda ()
         (beginend-message-mode 1)))

(Given "^I activate beginend-dired-mode$"
       (lambda ()
         (beginend-dired-mode 1)))

(Then "^I should be before \"\\([^\"]+\\)\"$"
      (lambda (string)
        (let ((message "Should have been before '%s' but is not (word at point is '%s')."))
          (cl-assert
           (looking-at string)
           nil message string (word-at-point)))))

(Then "^I should be after \"\\([^\"]+\\)\"$"
      (lambda (string)
        (let ((message "Should have been after '%s' but is not (word at point is '%s')."))
          (cl-assert
           (looking-back string)
           nil message string (word-at-point)))))

(Then "^I should be at beginning of buffer$"
      (lambda ()
        (cl-assert
         (bobp)
         nil "Is not at the beginning of the buffer")))

(Then "^I should be at end of buffer$"
      (lambda ()
        (cl-assert
         (eobp)
         nil "Is not at the end of the buffer")))

(Then "^I should be at line \\([0-9]+\\)$"
      (lambda (line)
        (let ((message "Should be at line '%s' but is at line '%s'"))
          (cl-assert
           (equal (string-to-number line) (line-number-at-pos))
           nil message line (line-number-at-pos)))))

(provide 'beginend-steps)

;;; beginend-steps.el ends here
