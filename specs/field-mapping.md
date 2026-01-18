# Feature: Field Mapping

Map 1Password item fields to auth-source keys.

## Consumed API

### `op` CLI

```
op item get <id> --format json --reveal
```

The `--reveal` flag returns actual secret values instead of `<concealed>`.

### Item Fields JSON Structure

```json
{
  "fields": [
    {
      "id": "username",
      "type": "STRING",
      "purpose": "USERNAME",
      "label": "username",
      "value": "alice@example.com"
    },
    {
      "id": "password",
      "type": "CONCEALED",
      "purpose": "PASSWORD",
      "label": "password",
      "value": "secret123"
    }
  ]
}
```

Key field attributes:
- `purpose`: `USERNAME`, `PASSWORD`, `NOTES`, or absent
- `type`: `STRING`, `CONCEALED`, `URL`, etc.

## Behavior

### Mapping Rules

| 1Password                | auth-source | Notes                     |
| ------------------------ | ----------- | ------------------------- |
| `purpose: "USERNAME"`    | `:user`     | Primary username field    |
| `purpose: "PASSWORD"`    | `:secret`   | Primary password field    |
| `type: "CONCEALED"`      | `:secret`   | Fallback for secure notes |
| matched URL hostname     | `:host`     | From search match         |
| URL port or search param | `:port`     | Optional                  |

### Item Type Handling

**Logins & API Credentials**: Use `purpose` field to identify username/password.

**Secure Notes**: No `purpose` fields. Use first field with `type: "CONCEALED"`.
If no CONCEALED field exists, item is not viable—exclude from results.

### Secret Wrapping

The `:secret` value must be a zero-arg closure, not the raw string. This
defers actual secret retrieval and provides security.

## Provided API

| Symbol                           | Type     | Description                        |
| -------------------------------- | -------- | ---------------------------------- |
| `auth-source-op--extract-username` | function | Get username from fields         |
| `auth-source-op--extract-secret`   | function | Get secret from fields           |
| `auth-source-op--map-item`         | function | Convert item to auth-source plist |

## Properties to Verify

1. Username extracted from `purpose=USERNAME` field
2. Password extracted from `purpose=PASSWORD` field
3. Falls back to first `type=CONCEALED` field when no PASSWORD purpose
4. Returns nil when no secret field found
5. Full item maps to plist with `:host`, `:user`, `:secret`
6. `:secret` is a callable closure, not raw string
7. Items without secrets excluded (return nil)

## Testing Strategy

- Test extraction functions with mock field arrays
- Test various item types: login, API credential, secure note
- Verify closure wrapping of secrets
- Test edge cases: missing fields, empty fields, multiple CONCEALED fields
