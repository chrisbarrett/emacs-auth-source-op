# Feature: Disambiguation UI

Present `completing-read` when multiple 1Password items match a search.

## Behavior

### Trigger Conditions

- Zero matches → return nil (no prompt)
- One match → return item directly (no prompt)
- Multiple matches → prompt user with `completing-read`

### Candidate Format

```
"Item Title (Vault Name)"
```

Include vault name to distinguish items with identical titles across vaults.

### User Cancellation

If user cancels with `C-g`, return nil (allow fallback to other backends).

## Provided API

| Symbol                            | Type     | Description                      |
| --------------------------------- | -------- | -------------------------------- |
| `auth-source-op--disambiguate`    | function | Select item from candidates      |
| `auth-source-op--format-candidate`| function | Format item for display          |

## Properties to Verify

1. Single item returns without prompting
2. Multiple items trigger `completing-read`
3. Candidates formatted as `"Title (Vault)"`
4. Selected item returned correctly
5. Empty list returns nil
6. User cancel (`C-g` / quit signal) returns nil

## Testing Strategy

- Mock `completing-read` to control selection
- Track whether `completing-read` was called for single vs multiple items
- Test quit signal handling with mock that signals `quit`
- Verify candidate formatting with various title/vault combinations
