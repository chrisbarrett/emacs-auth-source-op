# Feature: Item List Cache

Cache `op item list` results to avoid repeated slow CLI calls.

## Problem

`op item list` is slow (1-3 seconds). Without caching, every auth-source search
incurs this latency.

## Behavior

### Read-Through Cache

- First access fetches from `op` and stores in module variable
- Subsequent accesses return cached data
- Cache persists for Emacs session
- No automatic invalidation in v1

### Manual Refresh

Provide interactive command to clear and repopulate cache.

## Provided API

| Symbol                              | Type     | Description                    |
| ----------------------------------- | -------- | ------------------------------ |
| `auth-source-op--item-list-cache`   | variable | Cached item list (internal)    |
| `auth-source-op--get-item-list`     | function | Get cached list, fetch if needed |
| `auth-source-op-refresh-cache`      | command  | Clear and repopulate cache     |

## Properties to Verify

1. First call fetches data and populates cache
2. Subsequent calls return cached data without re-fetching
3. Fetch function called exactly once across multiple accesses
4. Refresh command clears cache and triggers new fetch
5. Cache contains expected data structure after fetch

## Testing Strategy

- Mock `auth-source-op--fetch-item-list` to track call count
- Verify single fetch across multiple `--get-item-list` calls
- Verify refresh resets and re-fetches
- No actual `op` CLI calls in tests
