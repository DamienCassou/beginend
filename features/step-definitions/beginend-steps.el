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

(provide 'beginend-steps)

;;; beginend-steps.el ends here
