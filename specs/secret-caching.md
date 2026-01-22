# Feature: Secret TTL Caching

Cache retrieved secrets in memory with a configurable time-to-live (TTL).

## Prerequisite

Requires item-list-cache feature.

## Problem

Currently, each call to a secret closure fetches from `op item get`. This is
slow (biometric prompt, CLI latency) and creates poor UX when the same
credential is needed multiple times in quick succession.

## Behavior

### TTL-Based Caching

- Secrets are cached after first retrieval
- Each item has an independent TTL
- Default TTL is 180 seconds (3 minutes)
- Accessing the secret resets its TTL (sliding window)

### Proactive Deletion

When TTL expires, the secret is immediately deleted from memory—not merely
marked stale. A timer fires and `setq`s the cached value to nil.

### Closure Registry

Closures are deduplicated via a registry keyed by item-id:

- Multiple searches for the same host return the same closure
- The registry holds closures, not secrets (secrets live in lexical bindings)
- Registry is cleared when the item-list cache is cleared

## Design: Let-Over-Lambda

Each closure captures its own state in lexical bindings:

```
┌─────────────────────────────────────┐
│ (let ((cached-secret nil)           │
│       (expiry-timer nil))           │
│   (lambda ()                        │
│     ;; fetch if needed              │
│     ;; reset timer on access        │
│     ;; return secret                │
│     ))                              │
└─────────────────────────────────────┘
```

Security properties:
- Secrets not directly accessible via any global variable
- Cannot enumerate cached secrets by inspecting the registry
- When TTL expires, secret is truly gone from memory
- Closure remains in registry (cheap), secret doesn't

## Provided API

| Symbol                             | Type     | Description                          |
| ---------------------------------- | -------- | ------------------------------------ |
| `auth-source-op-secret-ttl`        | defcustom | TTL in seconds (default 180)        |
| `auth-source-op--secret-closures`  | variable | Registry: item-id → closure          |
| `auth-source-op--make-secret-closure` | function | Get or create closure for item-id |

## Properties to Verify

1. First access fetches secret from `op` and caches it
2. Subsequent access within TTL returns cached value without fetching
3. Access resets the TTL timer
4. Secret is deleted from memory when TTL expires
5. Same item-id returns same closure (deduplication)
6. Clearing item-list cache also clears closure registry
7. Secrets not accessible by inspecting registry

## Testing Strategy

- Mock `auth-source-op--fetch-item` to track call count
- Verify single fetch across multiple closure calls within TTL
- Use `timer-set-time` or mock timers to test TTL expiry
- Verify secret is nil after TTL expires
- Verify closure reuse via `eq` comparison
- Verify registry cleared on cache clear
