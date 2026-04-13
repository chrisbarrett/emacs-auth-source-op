## Why

The auth-source-op package has no up-to-date specification. Previous specs were deleted because they drifted from the implementation. Documenting the existing behaviour as openspec specs establishes a baseline for future changes and catches regressions via spec-aware reviews.

## What Changes

- Add specs covering every public and internal capability of auth-source-op
- No code changes — specs describe what already exists

## Capabilities

### New Capabilities

- `op-cli-interface`: Invoking the `op` CLI via `call-process`, stderr separation, biometric retry loop, argument construction, error classification
- `item-list-cache`: Read-through item cache, vault filtering, single-vault optimisation, timestamp indexing, cache clear/refresh lifecycle
- `field-mapping`: Username and secret extraction from 1Password item fields, secret TTL closures with sliding-window expiry, deduplication registry
- `item-search`: Hostname matching against item URLs, title substring matching, combined host-or-title search
- `disambiguation-ui`: `completing-read` prompt for multiple matches, `inhibit-interaction` guard, single-item passthrough
- `auth-source-backend`: Backend registration, search dispatch, user filtering, max-results limiting, wildcard host rejection

### Modified Capabilities

(none — no existing specs)

## Impact

- New `openspec/specs/` directory with 6 spec files
- No code, test, or dependency changes
