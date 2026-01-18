# Feature: Smart Cache Invalidation

Automatically invalidate cached items when updated in 1Password.

## Prerequisite

Requires item-list-cache feature.

## Consumed API

Items include `updated_at` timestamp:

```json
{
  "id": "abc123",
  "title": "Gmail",
  "updated_at": "2024-01-15T10:30:00Z"
}
```

## Behavior

### Staleness Detection

Compare cached `updated_at` with fresh value. Item is stale if timestamps differ.

### Invalidation Approaches

**Option A: Timestamp Polling**
- Periodically fetch item list and compare timestamps
- Refresh cache if any tracked item has newer timestamp

**Option B: On-Demand Validation**
- Check timestamp only when item is accessed
- Fetch fresh item if cached version is stale

### Design Constraint

Must not interrupt user for explicit permission. If polling triggers biometric
prompts or is too slow, fall back to manual refresh only.

## Investigation Needed

- How fast is `op item list`? If <500ms, polling viable
- Does `op item get` trigger biometric for metadata only?
- Can we fetch timestamps without full item data?

## Provided API

| Symbol                               | Type     | Description                     |
| ------------------------------------ | -------- | ------------------------------- |
| `auth-source-op--item-stale-p`       | function | Compare item timestamps         |
| `auth-source-op--any-updated-p`      | function | Check if any items have changed |
| `auth-source-op--check-cache-staleness` | function | Refresh if stale             |

## Properties to Verify

1. Detects when cached timestamp differs from fresh
2. Same timestamps not considered stale
3. Detects changes in any item within list
4. No false positives when nothing changed
5. Staleness check triggers cache refresh when needed

## Testing Strategy

- Test timestamp comparison with mock cached/fresh data
- Test list-level change detection
- Mock fetch to verify refresh triggered on staleness
- No actual `op` CLI calls—all timestamp data mocked
