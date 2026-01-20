;;; auth-source-op.el --- Auth-source backend for 1Password -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Chris Barrett

;; Author: Chris Barrett <chris@walrus.cool>
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://github.com/chrisbarrett/emacs-auth-source-op
;; Version: 0.1.0

;;; Commentary:

;; Emacs auth-source backend for 1Password via the `op` CLI.
;;
;; This package enables `auth-source-search' to retrieve credentials from
;; 1Password, allowing Gnus, Forge, TRAMP, etc. to authenticate transparently.
;;
;; Requirements:
;; - `op' CLI in PATH
;; - 1Password desktop app (for biometric unlock)
;;
;; Usage:
;;   (require 'auth-source-op)
;;   (auth-source-op-enable)

;;; Code:

(require 'cl-lib)
(require 'json)

(defgroup auth-source-op nil
  "Auth-source backend for 1Password."
  :group 'auth-source
  :prefix "auth-source-op-")

(defcustom auth-source-op-retry-count 3
  "Maximum number of retries for biometric authentication failures."
  :type 'integer
  :group 'auth-source-op)

;;; Error Pattern Detection

(defconst auth-source-op--biometric-patterns
  '("authorization denied"
    "biometric"
    "authentication required"
    "touch id")
  "Patterns in stderr that indicate biometric authentication failure.")

(defconst auth-source-op--cancel-patterns
  '("user cancelled"
    "user canceled"
    "aborted"
    "operation was cancelled"
    "operation was canceled")
  "Patterns in stderr that indicate user cancelled authentication.")

(defun auth-source-op--biometric-failure-p (stderr)
  "Return non-nil if STDERR indicates a biometric authentication failure."
  (let ((stderr-lower (downcase stderr)))
    (cl-some (lambda (pattern)
               (string-match-p (regexp-quote pattern) stderr-lower))
             auth-source-op--biometric-patterns)))

(defun auth-source-op--user-cancelled-p (stderr)
  "Return non-nil if STDERR indicates user cancelled authentication."
  (let ((stderr-lower (downcase stderr)))
    (cl-some (lambda (pattern)
               (string-match-p (regexp-quote pattern) stderr-lower))
             auth-source-op--cancel-patterns)))

;;; op CLI Interface

(defun auth-source-op--check-op-available ()
  "Check if the `op' CLI is available.
Returns the path to `op' if found, nil otherwise.
Displays a warning if `op' is not found."
  (let ((op-path (executable-find "op")))
    (unless op-path
      (display-warning 'auth-source-op
                       "1Password CLI `op' not found in PATH. \
Install it from https://developer.1password.com/docs/cli/"
                       :warning))
    op-path))

(defun auth-source-op--call-op (&rest args)
  "Call the `op' CLI with ARGS.
Returns the parsed JSON output on success.
Retries on biometric failure up to `auth-source-op-retry-count' times.
Returns nil silently on user cancellation.
Signals an error on unexpected failures."
  (catch 'auth-source-op--return
    (unless (auth-source-op--check-op-available)
      (throw 'auth-source-op--return nil))
    (let ((retry-count 0)
          (max-retries auth-source-op-retry-count)
          result)
      (while (and (null result) (<= retry-count max-retries))
        (let* ((stderr-file (make-temp-file "op-stderr"))
               (command (mapconcat #'shell-quote-argument
                                   (cons "op" args)
                                   " "))
               (full-command (format "%s 2>%s"
                                     command
                                     (shell-quote-argument stderr-file)))
               (output (with-temp-buffer
                         (let ((exit-code (call-process-shell-command
                                           full-command nil t)))
                           (cons exit-code (buffer-string)))))
               (exit-code (car output))
               (stdout (cdr output))
               (stderr (with-temp-buffer
                         (insert-file-contents stderr-file)
                         (buffer-string))))
          (delete-file stderr-file)
          (cond
           ;; Success
           ((zerop exit-code)
            (setq result (if (string-empty-p stdout)
                             t
                           (condition-case nil
                               (json-read-from-string stdout)
                             (json-error stdout)))))
           ;; User cancelled - return nil immediately
           ((auth-source-op--user-cancelled-p stderr)
            (throw 'auth-source-op--return nil))
           ;; Biometric failure - retry
           ((auth-source-op--biometric-failure-p stderr)
            (setq retry-count (1+ retry-count))
            (when (> retry-count max-retries)
              (error "auth-source-op: Biometric authentication failed after %d attempts"
                     max-retries)))
           ;; Unexpected error
           (t
            (error "auth-source-op: `op' command failed: %s" stderr)))))
      result)))

(provide 'auth-source-op)
;;; auth-source-op.el ends here
