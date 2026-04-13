## ADDED Requirements

### Requirement: CLI invocation via call-process
The system SHALL invoke the `op` CLI using `call-process` with a `(BUFFER STDERR-FILE)` destination for native stdout/stderr separation. The system SHALL NOT use shell invocation or `call-process-shell-command`.

#### Scenario: Successful JSON command
- **WHEN** `auth-source-op--call-op` is called with valid arguments
- **THEN** the `op` executable is invoked via `call-process` with arguments passed directly (no shell quoting)
- **THEN** stdout is parsed as JSON and returned

#### Scenario: Successful command with empty output
- **WHEN** `op` exits 0 with empty stdout
- **THEN** `call-op` returns the symbol `t`

#### Scenario: JSON parse failure
- **WHEN** `op` exits 0 but stdout is not valid JSON
- **THEN** `call-op` returns the raw stdout string

### Requirement: Argument construction
The system SHALL build CLI arguments via `auth-source-op--build-args`, appending `--account=ACCOUNT` when `auth-source-op-account` is non-nil. Nil values in the argument list SHALL be filtered out.

#### Scenario: No account configured
- **WHEN** `auth-source-op-account` is nil
- **THEN** no `--account` flag is included in the argument list

#### Scenario: Account configured
- **WHEN** `auth-source-op-account` is `"my-account"`
- **THEN** `--account=my-account` is appended to the argument list

### Requirement: Biometric retry
The system SHALL retry on biometric authentication failure up to `auth-source-op-retry-count` times. Biometric failure is detected by matching stderr against known patterns (case-insensitive): "authorization denied", "biometric", "authentication required", "touch id".

#### Scenario: Transient biometric failure then success
- **WHEN** `op` fails with "authorization denied" stderr, then succeeds on retry
- **THEN** `call-op` returns the successful result

#### Scenario: Biometric retries exhausted
- **WHEN** `op` fails with biometric errors for all retry attempts
- **THEN** `call-op` displays a warning and returns nil

### Requirement: User cancellation detection
The system SHALL detect user cancellation by matching stderr against patterns: "user cancelled", "user canceled", "aborted", "operation was cancelled", "operation was canceled". Detection SHALL be case-insensitive.

#### Scenario: User cancels authentication
- **WHEN** `op` exits non-zero with "user cancelled" in stderr
- **THEN** `call-op` returns nil immediately without retry or warning

### Requirement: Unexpected error handling
The system SHALL display a warning via `display-warning` and return nil for non-zero exit codes that are neither biometric failures nor user cancellations.

#### Scenario: Unknown op error
- **WHEN** `op` exits non-zero with unrecognised stderr
- **THEN** a warning is displayed containing the stderr content
- **THEN** `call-op` returns nil

### Requirement: Executable availability check
The system SHALL verify `op` is available via `executable-find` before each invocation. The executable name is configurable via `auth-source-op-executable`.

#### Scenario: op not in PATH
- **WHEN** `executable-find` returns nil for the configured executable
- **THEN** a warning is displayed and `call-op` returns nil

### Requirement: Stderr temp file cleanup
The system SHALL clean up the stderr temp file in an `unwind-protect` form, ensuring cleanup even on non-local exits.

#### Scenario: Error during processing
- **WHEN** an error occurs after the temp file is created
- **THEN** the temp file is deleted via the `unwind-protect` cleanup form
