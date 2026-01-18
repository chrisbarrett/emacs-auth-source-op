# Feature: Cache Management UI

Interactive commands for viewing and managing the credential cache.

## Prerequisite

Requires item-list-cache feature.

## Commands

### `M-x auth-source-op-cache-list`

Display cached items in a dedicated buffer with redacted secrets:

```
auth-source-op cached items:
───────────────────────────────────────
  Gmail (Personal)
    URL: mail.google.com
    User: alice@gmail.com
    Secret: ••••••••

  GitHub API (Work)
    URL: api.github.com
    User: alice
    Secret: ••••••••
───────────────────────────────────────
3 items cached
```

### `M-x auth-source-op-cache-refresh`

Clear and repopulate entire cache from 1Password.

### `M-x auth-source-op-cache-remove`

Remove specific item from cache via `completing-read` selection.

### `M-x auth-source-op-cache-clear`

Clear entire cache (both item list and individual items).

## Security Constraint

Never display actual secrets. Use `••••••••` placeholder. Actual secrets only
retrieved via `:secret` closure when explicitly requested by caller.

## Provided API

| Symbol                          | Type    | Description                     |
| ------------------------------- | ------- | ------------------------------- |
| `auth-source-op-cache-list`     | command | Display cache in buffer         |
| `auth-source-op-cache-refresh`  | command | Clear and repopulate cache      |
| `auth-source-op-cache-remove`   | command | Remove single item from cache   |
| `auth-source-op-cache-clear`    | command | Clear all cached data           |

## Properties to Verify

1. `cache-list` creates buffer with expected name
2. `cache-list` never displays actual secret values
3. `cache-list` shows redacted placeholder for secrets
4. `cache-clear` empties all cache variables
5. `cache-remove` deletes only selected item
6. `cache-refresh` clears and re-fetches

## Testing Strategy

- Verify buffer creation and content for `cache-list`
- Search buffer content to confirm secrets not present
- Search buffer content to confirm placeholder present
- Check cache state after clear/remove/refresh operations
- Mock `completing-read` for `cache-remove` selection
- Mock fetch for `cache-refresh` verification
