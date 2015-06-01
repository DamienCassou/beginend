(require 'f)

(defvar beginend-support-path
  (f-dirname load-file-name))

(defvar beginend-features-path
  (f-parent beginend-support-path))

(defvar beginend-root-path
  (f-parent beginend-features-path))

(add-to-list 'load-path beginend-root-path)

(require 'beginend)
(require 'espuds)
(require 'ert)

(defvar beginend--test-tempdir)

(Setup
 ;; Before anything has run
 ;; Create a temporary directory for dired-mode tests
 (setq beginend--test-tempdir (make-temp-file "beginend-test" t))
 (f-touch (f-expand "file1" beginend--test-tempdir))
 (f-touch (f-expand "file2" beginend--test-tempdir)))

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 (when beginend--test-tempdir
   (f-delete beginend--test-tempdir t)))
