;;; auth-source-op-test.el --- Tests for auth-source-op -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for error handling in auth-source-op.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add parent directory to load-path
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." dir)))

(require 'auth-source-op)

;;; Test Helpers

(defvar auth-source-op-test--mock-executable-find nil
  "Mock return value for `executable-find'.")

(defvar auth-source-op-test--mock-call-results nil
  "List of (exit-code stdout stderr) for successive mock calls.")

(defvar auth-source-op-test--mock-call-count 0
  "Count of mock CLI calls made.")

(defmacro auth-source-op-test--with-mocks (&rest body)
  "Execute BODY with mocked functions."
  (declare (indent 0))
  `(let ((auth-source-op-test--mock-call-count 0))
     (cl-letf (((symbol-function 'executable-find)
                (lambda (_cmd)
                  auth-source-op-test--mock-executable-find))
               ((symbol-function 'call-process-shell-command)
                (lambda (_cmd &optional _infile buffer &rest _args)
                  (let* ((result (nth auth-source-op-test--mock-call-count
                                      auth-source-op-test--mock-call-results))
                         (exit-code (nth 0 result))
                         (stdout (nth 1 result))
                         (stderr (nth 2 result)))
                    (setq auth-source-op-test--mock-call-count
                          (1+ auth-source-op-test--mock-call-count))
                    ;; Write stderr to temp file (which was created by the real code)
                    ;; Actually, we need to mock differently - let's write to a known file
                    (when (and buffer (bufferp buffer))
                      (with-current-buffer buffer
                        (insert stdout)))
                    (when buffer
                      (with-current-buffer (if (bufferp buffer) buffer
                                             (current-buffer))
                        (insert stdout)))
                    exit-code)))
               ((symbol-function 'make-temp-file)
                (lambda (_prefix &rest _args)
                  (let ((f (expand-file-name
                            (format "op-test-stderr-%d"
                                    auth-source-op-test--mock-call-count)
                            temporary-file-directory)))
                    ;; Pre-write the stderr content
                    (let* ((result (nth auth-source-op-test--mock-call-count
                                        auth-source-op-test--mock-call-results))
                           (stderr (or (nth 2 result) "")))
                      (with-temp-file f
                        (insert stderr)))
                    f))))
       ,@body)))

;;; Tests for Error Pattern Detection

(ert-deftest auth-source-op-test-biometric-failure-detection ()
  "Test detection of biometric failure patterns."
  (should (auth-source-op--biometric-failure-p "authorization denied"))
  (should (auth-source-op--biometric-failure-p "AUTHORIZATION DENIED"))
  (should (auth-source-op--biometric-failure-p "biometric authentication failed"))
  (should (auth-source-op--biometric-failure-p "Touch ID failed"))
  (should (auth-source-op--biometric-failure-p "authentication required"))
  (should-not (auth-source-op--biometric-failure-p "item not found"))
  (should-not (auth-source-op--biometric-failure-p "")))

(ert-deftest auth-source-op-test-user-cancelled-detection ()
  "Test detection of user cancellation patterns."
  (should (auth-source-op--user-cancelled-p "user cancelled"))
  (should (auth-source-op--user-cancelled-p "USER CANCELLED"))
  (should (auth-source-op--user-cancelled-p "user canceled"))
  (should (auth-source-op--user-cancelled-p "operation was aborted"))
  (should (auth-source-op--user-cancelled-p "operation was cancelled by user"))
  (should-not (auth-source-op--user-cancelled-p "authentication failed"))
  (should-not (auth-source-op--user-cancelled-p "")))

;;; Tests for op Availability Check

(ert-deftest auth-source-op-test-check-op-available-found ()
  "Test that check returns path when op is found."
  (let ((auth-source-op-test--mock-executable-find "/usr/local/bin/op"))
    (auth-source-op-test--with-mocks
      (should (equal "/usr/local/bin/op"
                     (auth-source-op--check-op-available))))))

(ert-deftest auth-source-op-test-check-op-available-not-found ()
  "Test that check returns nil and warns when op is not found."
  (let ((auth-source-op-test--mock-executable-find nil)
        (warning-displayed nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest _args)
                 (setq warning-displayed t))))
      (auth-source-op-test--with-mocks
        (should-not (auth-source-op--check-op-available))
        (should warning-displayed)))))

;;; Tests for op CLI Calls

(ert-deftest auth-source-op-test-call-op-success ()
  "Test successful op CLI call."
  (let ((auth-source-op-test--mock-executable-find "/usr/local/bin/op")
        (auth-source-op-test--mock-call-results
         '((0 "{\"id\": \"abc123\", \"title\": \"Test\"}" ""))))
    (auth-source-op-test--with-mocks
      (let ((result (auth-source-op--call-op "item" "get" "test")))
        (should result)
        (should (equal "abc123" (alist-get 'id result)))
        (should (equal "Test" (alist-get 'title result)))))))

(ert-deftest auth-source-op-test-call-op-not-available ()
  "Test that call returns nil when op is not available."
  (let ((auth-source-op-test--mock-executable-find nil))
    (cl-letf (((symbol-function 'display-warning) #'ignore))
      (auth-source-op-test--with-mocks
        (should-not (auth-source-op--call-op "item" "list"))))))

(ert-deftest auth-source-op-test-call-op-user-cancelled ()
  "Test that user cancellation returns nil immediately without retry."
  (let ((auth-source-op-test--mock-executable-find "/usr/local/bin/op")
        (auth-source-op-test--mock-call-results
         '((1 "" "error: user cancelled"))))
    (auth-source-op-test--with-mocks
      (should-not (auth-source-op--call-op "item" "get" "test"))
      ;; Should only call once (no retry on cancel)
      (should (= 1 auth-source-op-test--mock-call-count)))))

(ert-deftest auth-source-op-test-call-op-biometric-retry ()
  "Test that biometric failure triggers retry."
  (let ((auth-source-op-test--mock-executable-find "/usr/local/bin/op")
        (auth-source-op-test--mock-call-results
         '((1 "" "error: authorization denied")
           (1 "" "error: authorization denied")
           (0 "{\"id\": \"test\"}" "")))
        (auth-source-op-retry-count 3))
    (auth-source-op-test--with-mocks
      (let ((result (auth-source-op--call-op "item" "get" "test")))
        (should result)
        (should (equal "test" (alist-get 'id result)))
        ;; Should have retried twice before succeeding
        (should (= 3 auth-source-op-test--mock-call-count))))))

(ert-deftest auth-source-op-test-call-op-biometric-exhausted ()
  "Test that exhausted biometric retries signal error."
  (let ((auth-source-op-test--mock-executable-find "/usr/local/bin/op")
        (auth-source-op-test--mock-call-results
         '((1 "" "error: authorization denied")
           (1 "" "error: authorization denied")
           (1 "" "error: authorization denied")
           (1 "" "error: authorization denied")))
        (auth-source-op-retry-count 3))
    (auth-source-op-test--with-mocks
      (should-error (auth-source-op--call-op "item" "get" "test")
                    :type 'error))))

(ert-deftest auth-source-op-test-call-op-unexpected-error ()
  "Test that unexpected errors signal with stderr content."
  (let ((auth-source-op-test--mock-executable-find "/usr/local/bin/op")
        (auth-source-op-test--mock-call-results
         '((1 "" "error: vault not found"))))
    (auth-source-op-test--with-mocks
      (should-error (auth-source-op--call-op "vault" "list")
                    :type 'error))))

(ert-deftest auth-source-op-test-call-op-empty-output ()
  "Test that empty output returns t (success with no data)."
  (let ((auth-source-op-test--mock-executable-find "/usr/local/bin/op")
        (auth-source-op-test--mock-call-results
         '((0 "" ""))))
    (auth-source-op-test--with-mocks
      (should (eq t (auth-source-op--call-op "signout"))))))

;;; Tests for Item List Cache

(ert-deftest auth-source-op-test-cache-get-fetches-on-first-access ()
  "Test that cache-get fetches items on first access."
  (let ((auth-source-op-test--mock-executable-find "/usr/local/bin/op")
        (auth-source-op-test--mock-call-results
         '((0 "[{\"id\": \"item1\"}, {\"id\": \"item2\"}]" "")))
        (auth-source-op--item-cache nil)
        (auth-source-op--cache-timestamp nil))
    (auth-source-op-test--with-mocks
      (let ((items (auth-source-op--cache-get)))
        (should items)
        (should (= 2 (length items)))
        (should (= 1 auth-source-op-test--mock-call-count))))))

(ert-deftest auth-source-op-test-cache-get-returns-cached-on-subsequent ()
  "Test that cache-get returns cached items without fetching again."
  (let ((auth-source-op-test--mock-executable-find "/usr/local/bin/op")
        (auth-source-op-test--mock-call-results
         '((0 "[{\"id\": \"item1\"}]" "")))
        (auth-source-op--item-cache nil)
        (auth-source-op--cache-timestamp nil))
    (auth-source-op-test--with-mocks
      ;; First access fetches
      (auth-source-op--cache-get)
      (should (= 1 auth-source-op-test--mock-call-count))
      ;; Second access uses cache
      (auth-source-op--cache-get)
      (should (= 1 auth-source-op-test--mock-call-count)))))

(ert-deftest auth-source-op-test-cache-refresh-forces-fetch ()
  "Test that cache-refresh always fetches fresh data."
  (let ((auth-source-op-test--mock-executable-find "/usr/local/bin/op")
        (auth-source-op-test--mock-call-results
         '((0 "[{\"id\": \"item1\"}]" "")
           (0 "[{\"id\": \"item1\"}, {\"id\": \"item2\"}]" "")))
        (auth-source-op--item-cache nil)
        (auth-source-op--cache-timestamp nil))
    (auth-source-op-test--with-mocks
      ;; First fetch
      (auth-source-op--cache-refresh)
      (should (= 1 (length auth-source-op--item-cache)))
      ;; Force refresh
      (auth-source-op--cache-refresh)
      (should (= 2 (length auth-source-op--item-cache)))
      (should (= 2 auth-source-op-test--mock-call-count)))))

(ert-deftest auth-source-op-test-cache-clear-empties-cache ()
  "Test that cache-clear empties the cache."
  (let ((auth-source-op--item-cache '(((id . "test"))))
        (auth-source-op--cache-timestamp (current-time)))
    (auth-source-op--cache-clear)
    (should-not auth-source-op--item-cache)
    (should-not auth-source-op--cache-timestamp)))

(ert-deftest auth-source-op-test-cache-sets-timestamp ()
  "Test that cache refresh sets the timestamp."
  (let ((auth-source-op-test--mock-executable-find "/usr/local/bin/op")
        (auth-source-op-test--mock-call-results
         '((0 "[{\"id\": \"item1\"}]" "")))
        (auth-source-op--item-cache nil)
        (auth-source-op--cache-timestamp nil))
    (auth-source-op-test--with-mocks
      (auth-source-op--cache-refresh)
      (should auth-source-op--cache-timestamp)
      (should (time-less-p (time-subtract (current-time) 5)
                           auth-source-op--cache-timestamp)))))

(ert-deftest auth-source-op-test-cache-handles-fetch-failure ()
  "Test that cache handles fetch failure gracefully."
  (let ((auth-source-op-test--mock-executable-find nil)
        (auth-source-op--item-cache nil)
        (auth-source-op--cache-timestamp nil))
    (cl-letf (((symbol-function 'display-warning) #'ignore))
      (auth-source-op-test--with-mocks
        (should-not (auth-source-op--cache-refresh))
        (should-not auth-source-op--item-cache)))))

(ert-deftest auth-source-op-test-cache-converts-vector-to-list ()
  "Test that cache converts JSON vector to list."
  (let ((auth-source-op-test--mock-executable-find "/usr/local/bin/op")
        (auth-source-op-test--mock-call-results
         '((0 "[{\"id\": \"item1\"}, {\"id\": \"item2\"}]" "")))
        (auth-source-op--item-cache nil)
        (auth-source-op--cache-timestamp nil))
    (auth-source-op-test--with-mocks
      (let ((items (auth-source-op--cache-get)))
        ;; Should be a list, not a vector
        (should (listp items))
        (should-not (vectorp items))))))

(provide 'auth-source-op-test)
;;; auth-source-op-test.el ends here
