# Feature: Auth-Source Backend Registration

Register `auth-source-op` as an auth-source backend so Emacs packages (Gnus,
Forge, etc.) can retrieve credentials via standard `auth-source-search`.

## Dependencies

- `op` CLI installed and in PATH
- 1Password desktop app (biometric auth)

## Consumed API

Reference: `auth-source-pass.el` in Emacs core.

### Backend Registration Pattern

1. Define backend variable using `auth-source-backend` constructor
2. Register parser function via `auth-source-backend-parser-functions` hook
3. Parser returns backend when entry matches our symbol (e.g., `'1password`)

### Search Function Signature

```
auth-source-op-search (&rest spec &key backend type host user port require max &allow-other-keys)
```

Parameters from auth-source:

| Parameter | Type               | Description                          |
| --------- | ------------------ | ------------------------------------ |
| `host`    | string, list, or t | Hostname(s) to match                 |
| `user`    | string or t        | Username to match                    |
| `port`    | string or t        | Port/protocol to match               |
| `max`     | integer            | Maximum results                      |
| `require` | list               | Keys that must be present in results |

### Return Format

List of plists with `:host`, `:user`, `:port`, `:secret`. The `:secret` value
must be a zero-arg closure returning the actual secret (deferred retrieval).

## Provided API

| Symbol                         | Type     | Description                      |
| ------------------------------ | -------- | -------------------------------- |
| `auth-source-op-backend`       | variable | The backend instance             |
| `auth-source-op-backend-parse` | function | Parser for backend-parser-functions |
| `auth-source-op-search`        | function | Main search entry point          |
| `auth-source-op-enable`        | command  | Add backend to `auth-sources`    |

## Properties to Verify

1. Backend variable exists and has correct type slot
2. Parser registered in `auth-source-backend-parser-functions`
3. Parser returns backend for our symbol, nil for others
4. Enable command adds symbol to `auth-sources`
5. Search with `host=nil` returns nil
6. Search with `host=t` warns and returns nil (wildcards unsupported)
7. Returned `:secret` is callable, not raw string

## Testing Strategy

- Mock `auth-source-op--do-search` to isolate backend registration logic
- Verify parser discrimination with multiple entry types
- Check `auth-sources` mutation after enable
- Verify warning mechanism for unsupported wildcards
