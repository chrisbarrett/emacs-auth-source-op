## ADDED Requirements

### Requirement: Single item passthrough
When the input list contains exactly one item, `auth-source-op--disambiguate` SHALL return it without prompting.

#### Scenario: One item
- **WHEN** items list has one element
- **THEN** that element is returned directly

### Requirement: Empty list
When the input list is nil, `auth-source-op--disambiguate` SHALL return nil.

#### Scenario: No items
- **WHEN** items list is nil
- **THEN** nil is returned

### Requirement: Interactive disambiguation
When multiple items are present and interaction is allowed, the system SHALL prompt the user via `completing-read` with item titles and hostnames.

#### Scenario: User selects an item
- **WHEN** multiple items exist and user selects one via `completing-read`
- **THEN** the selected item is returned

#### Scenario: User cancels
- **WHEN** user quits `completing-read`
- **THEN** nil is returned

### Requirement: Non-interactive fallback
When `inhibit-interaction` is non-nil, the system SHALL return the first item without prompting. This prevents blocking in non-interactive callers such as Forge or Gnus.

#### Scenario: inhibit-interaction is set
- **WHEN** `inhibit-interaction` is non-nil and multiple items exist
- **THEN** the first item is returned without calling `completing-read`

### Requirement: Display formatting
Each item SHALL be displayed as "TITLE (HOSTNAME)" when a URL hostname is available, or just "TITLE" otherwise. Items without a title SHALL display as "Untitled".

#### Scenario: Item with URL
- **WHEN** item has title "GitHub" and URL hostname "github.com"
- **THEN** display string is "GitHub (github.com)"

#### Scenario: Item without URL
- **WHEN** item has title "API Key" and no URLs
- **THEN** display string is "API Key"
