# Feature: Item Search

Search 1Password items by URL hostname or title.

## Consumed API

### `op` CLI

No direct URL search available. Must use:

- `op item list --format json` → all items with metadata (includes `urls` array)
- `op item get <id> --format json` → full item with fields

### Item List JSON Structure

```json
{
  "id": "abc123",
  "title": "Gmail",
  "category": "LOGIN",
  "vault": { "id": "v1", "name": "Personal" },
  "urls": [{ "href": "https://mail.google.com/" }],
  "updated_at": "2024-01-15T10:30:00Z"
}
```

## Behavior

Given a host string (e.g., `"smtp.gmail.com"`):

1. Get cached item list (see item-list-cache spec)
2. Filter items where any `urls[].href` hostname matches exactly
3. Also include items where `title` equals the host string
4. Return matching item summaries

### Matching Rules

- **Strict hostname match** — `smtp.gmail.com` ≠ `mail.google.com`
- **Port ignored** — `smtp.gmail.com:587` matches `smtp.gmail.com`
- **Title fallback** — match if `title` equals host string exactly

### Hostname Extraction

Extract hostname from URL using `url-generic-parse-url`. Handle:
- Full URLs: `https://example.com/path` → `example.com`
- URLs with ports: `http://example.com:8080/` → `example.com`
- Bare hostnames: `example.com` → `example.com`

## Provided API

| Symbol                              | Type     | Description                     |
| ----------------------------------- | -------- | ------------------------------- |
| `auth-source-op--filter-items-by-host` | function | Filter item list by hostname |
| `auth-source-op--extract-hostname`  | function | Extract hostname from URL string |

## Properties to Verify

1. URL hostname extraction handles full URLs, ports, bare hostnames
2. Items with matching URL hostname are included
3. Items with matching title are included
4. Non-matching items excluded
5. Strict matching: different subdomains don't match
6. Empty result when no matches

## Testing Strategy

- Test hostname extraction with various URL formats
- Test filtering with mock item lists containing various URL patterns
- Verify strict matching by testing near-misses (subdomain differences)
- No `op` CLI calls needed—all tests use mock data
