# Feature: Error Handling

Handle `op` CLI failures gracefully.

## Error Conditions

| Condition                  | Detection                        | Action                           |
| -------------------------- | -------------------------------- | -------------------------------- |
| `op` not in PATH           | `executable-find` returns nil    | Warning buffer, return nil       |
| Biometric fails/timeout    | Non-zero exit + specific stderr  | Retry N times, then signal error |
| No matching items          | Empty search results             | Return nil (silent)              |
| User cancels auth          | Non-zero exit + cancel pattern   | Return nil (silent)              |
| Unexpected `op` error      | Non-zero exit + other stderr     | Signal error                     |

## Behavior

### `op` Not Found

Display warning via `display-warning` to `*Warnings*` buffer. Return nil to
allow fallback to other auth-source backends.

### Biometric Retry

Configurable retry count (defcustom). On each biometric failure, retry up to
limit. After exhausting retries, signal descriptive error.

### Error Pattern Detection

Parse stderr for known patterns:
- Biometric failure: "authorization denied", "biometric"
- User cancelled: "user cancelled", "aborted"

## Provided API

| Symbol                              | Type      | Description                      |
| ----------------------------------- | --------- | -------------------------------- |
| `auth-source-op-retry-count`        | defcustom | Max biometric retries            |
| `auth-source-op--check-op-available`| function  | Check if `op` CLI exists         |
| `auth-source-op--call-op`           | function  | Call `op` with retry logic       |
| `auth-source-op--biometric-failure-p` | function | Detect biometric failure       |
| `auth-source-op--user-cancelled-p`  | function  | Detect user cancellation         |

## Properties to Verify

1. Missing `op` displays warning and returns nil
2. Biometric failure patterns correctly detected
3. User cancellation patterns correctly detected
4. Retries on biometric failure up to configured count
5. User cancel returns nil immediately (no retry)
6. Unexpected errors signal with stderr content
7. Success returns parsed result

## Testing Strategy

- Mock `executable-find` to simulate missing `op`
- Mock CLI call to return various exit codes and stderr patterns
- Track retry count to verify retry logic
- Verify immediate return on user cancel (no wasted retries)
- Test error signaling for unexpected failures
