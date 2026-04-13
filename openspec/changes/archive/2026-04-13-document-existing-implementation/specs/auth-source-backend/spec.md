## ADDED Requirements

### Requirement: Backend registration
`auth-source-op-enable` SHALL add the backend parser to `auth-source-backend-parser-functions` and add `'1password` to `auth-sources`. It SHALL error if the `op` CLI is not available.

#### Scenario: Successful enable
- **WHEN** `op` is found in PATH
- **THEN** the backend parser is registered and `'1password` is added to `auth-sources`

#### Scenario: op not available
- **WHEN** `op` is not found in PATH
- **THEN** `user-error` is signalled

### Requirement: Backend parser
`auth-source-op--backend-parse` SHALL return the backend object when entry is the symbol `1password`, and nil for any other entry.

#### Scenario: Matching entry
- **WHEN** entry is `'1password`
- **THEN** the backend object is returned

#### Scenario: Non-matching entry
- **WHEN** entry is `"~/.authinfo"`
- **THEN** nil is returned

### Requirement: Wildcard host rejection
The search function SHALL return nil immediately when host is `t` (wildcard).

#### Scenario: Wildcard host
- **WHEN** `auth-source-op--search` is called with `:host t`
- **THEN** nil is returned without searching

### Requirement: Host as title fallback
The search function SHALL pass the host value as both host and title to `search-items`, so items can match by title when host is not a URL hostname.

#### Scenario: Title-only match
- **WHEN** searching for `:host "GitHub"` and an item has title "GitHub" but no matching URL
- **THEN** the item is found via title matching

### Requirement: Disambiguation on single max
When `max` is 1 (or defaulted) and multiple items match, the search function SHALL invoke disambiguation to select one item.

#### Scenario: Multiple matches with max 1
- **WHEN** 3 items match and max is 1
- **THEN** `disambiguate` is called with all 3 items

### Requirement: User filtering
When `:user` is specified and is not `t`, results SHALL be filtered to only include items whose extracted username matches the specified user.

#### Scenario: User matches
- **WHEN** searching with `:user "alice"` and item has username "alice"
- **THEN** the item is included

#### Scenario: User does not match
- **WHEN** searching with `:user "alice"` and item has username "bob"
- **THEN** the item is excluded

#### Scenario: User is nil or t
- **WHEN** searching with `:user nil` or `:user t`
- **THEN** no user filtering is applied

### Requirement: Max results limiting
The search function SHALL return at most `max` results (defaulting to 1).

#### Scenario: Max limits results
- **WHEN** 5 items match and max is 2
- **THEN** at most 2 results are returned

### Requirement: Error resilience
The search function SHALL catch errors, display a warning, and return nil rather than propagating exceptions to `auth-source-search` callers.

#### Scenario: Internal error
- **WHEN** an error occurs during search
- **THEN** a warning is displayed and nil is returned
