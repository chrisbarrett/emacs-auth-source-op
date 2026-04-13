## ADDED Requirements

### Requirement: Username extraction
The system SHALL extract usernames from 1Password item fields by matching field labels or IDs (case-insensitive) against: "username", "user", "email", "login", "account".

#### Scenario: Username found by label
- **WHEN** an item has a field with label "username" and a non-nil value
- **THEN** `extract-username` returns that value

#### Scenario: Username found by ID
- **WHEN** an item has a field with id "email" and a non-nil value
- **THEN** `extract-username` returns that value

#### Scenario: No username field
- **WHEN** no field matches any username field name
- **THEN** `extract-username` returns nil

### Requirement: Secret extraction
The system SHALL extract secrets from 1Password item fields by matching field labels or IDs against: "password", "secret", "credential", "token", "api_key", "apikey", "key". Additionally, fields with type "CONCEALED" SHALL match when the purpose is `password`.

#### Scenario: Secret found by label
- **WHEN** an item has a field labelled "password" with a value
- **THEN** the field value is returned

#### Scenario: Secret found by CONCEALED type
- **WHEN** an item has a field with type "CONCEALED" and purpose is `password`
- **THEN** the field value is returned even if the label does not match any known name

### Requirement: Secret TTL closure
The system SHALL wrap secrets in zero-arg closures that implement TTL-based caching. The secret is fetched from 1Password on first invocation, cached in a lexical binding, and proactively deleted when `auth-source-op-secret-ttl` seconds elapse.

#### Scenario: First invocation fetches secret
- **WHEN** the secret closure is called for the first time
- **THEN** `op item get` is invoked to fetch the full item and extract the secret

#### Scenario: Subsequent invocation returns cached secret
- **WHEN** the closure is called again before TTL expires
- **THEN** the cached secret is returned without invoking `op`

#### Scenario: TTL expiry clears secret
- **WHEN** `auth-source-op-secret-ttl` seconds elapse without access
- **THEN** the cached secret is set to nil and the next call re-fetches from 1Password

#### Scenario: Access resets TTL (sliding window)
- **WHEN** the closure is called before TTL expires
- **THEN** the TTL timer is cancelled and restarted from the current time

### Requirement: Secret closure deduplication
The system SHALL maintain a registry (`auth-source-op--secret-closures`) mapping item IDs to closures. Requesting a closure for the same item ID SHALL return the existing closure.

#### Scenario: Same item ID returns same closure
- **WHEN** `make-secret-closure` is called twice with the same item ID
- **THEN** the same closure object is returned both times

#### Scenario: Different item IDs return different closures
- **WHEN** `make-secret-closure` is called with two different item IDs
- **THEN** two distinct closure objects are returned

### Requirement: Item-to-auth-source mapping
The system SHALL map 1Password items to auth-source result plists via `fetch-and-map-item`. The plist SHALL contain `:host` (from URL hostname or item title), `:port` (nil), `:user` (from username extraction), and `:secret` (a TTL closure).

#### Scenario: Item with URL
- **WHEN** an item has a URL with hostname "github.com"
- **THEN** `:host` is "github.com"

#### Scenario: Item without URL
- **WHEN** an item has no URLs but title "My Service"
- **THEN** `:host` is "My Service"

#### Scenario: Fetch failure
- **WHEN** `op item get` fails for the item
- **THEN** `fetch-and-map-item` returns nil
