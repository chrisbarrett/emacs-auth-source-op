## ADDED Requirements

### Requirement: Read-through cache
The system SHALL maintain a cached list of 1Password item summaries. On first access via `auth-source-op--cache-get`, the cache SHALL be populated by calling `op item list --format=json`.

#### Scenario: First access populates cache
- **WHEN** `cache-get` is called with an empty cache
- **THEN** `op item list` is invoked and results are stored

#### Scenario: Subsequent access returns cached data
- **WHEN** `cache-get` is called after cache is populated
- **THEN** the cached list is returned without invoking `op`

### Requirement: Vault filtering
The system SHALL filter items by `auth-source-op-vaults` when configured. Matching SHALL compare against both vault name (case-insensitive) and vault ID (exact match).

#### Scenario: Single vault configured
- **WHEN** `auth-source-op-vaults` contains exactly one vault
- **THEN** `op item list --vault=VAULT` is used for server-side filtering

#### Scenario: Multiple vaults configured
- **WHEN** `auth-source-op-vaults` contains multiple vaults
- **THEN** all items are fetched and filtered client-side by vault name or ID

#### Scenario: No vaults configured
- **WHEN** `auth-source-op-vaults` is nil
- **THEN** all items from all vaults are included

### Requirement: Timestamp indexing
The system SHALL build a hash table mapping item IDs to their `updated_at` timestamps when the cache is refreshed.

#### Scenario: Cache refresh builds index
- **WHEN** `cache-refresh` completes successfully
- **THEN** `auth-source-op--item-timestamps` contains a hash table keyed by item ID with `updated_at` values

### Requirement: Cache clear
The system SHALL provide `auth-source-op--cache-clear` which resets the item cache, cache timestamp, timestamp index, and secret closure registry to nil.

#### Scenario: Clear resets all state
- **WHEN** `cache-clear` is called
- **THEN** `auth-source-op--item-cache`, `auth-source-op--cache-timestamp`, `auth-source-op--item-timestamps`, and `auth-source-op--secret-closures` are all nil

### Requirement: Vector-to-list normalisation
The system SHALL convert JSON arrays (Emacs vectors) to lists when processing `op` output.

#### Scenario: op returns a JSON array
- **WHEN** `op item list` returns a vector of items
- **THEN** the cache stores items as a list

### Requirement: Interactive cache management
The system SHALL provide three interactive commands: `auth-source-op-refresh-cache` (clear and re-fetch), `auth-source-op-cache-clear` (clear only), and `auth-source-op-cache-list` (display cached items in a read-only buffer).

#### Scenario: Refresh cache command
- **WHEN** user invokes `auth-source-op-refresh-cache`
- **THEN** the cache is cleared, re-fetched, and a message reports the item count

#### Scenario: Cache list display
- **WHEN** user invokes `auth-source-op-cache-list`
- **THEN** a `*1Password Cache*` buffer is displayed in `special-mode` showing item titles, hosts, IDs, and update timestamps
