## ADDED Requirements

### Requirement: Hostname extraction
The system SHALL extract hostnames from URLs by parsing with `url-generic-parse-url`. URLs without a protocol SHALL have "https://" prepended. Hostnames SHALL be lowercased.

#### Scenario: Full URL
- **WHEN** URL is "https://GitHub.com/login"
- **THEN** extracted hostname is "github.com"

#### Scenario: URL without protocol
- **WHEN** URL is "example.com"
- **THEN** extracted hostname is "example.com"

#### Scenario: Invalid URL
- **WHEN** URL is nil or unparseable
- **THEN** extraction returns nil

### Requirement: Host matching
The system SHALL match items against a target host by comparing the target (lowercased) against hostnames extracted from item URLs. Matching SHALL be strict equality — no wildcard or subdomain inference.

#### Scenario: Exact host match
- **WHEN** item has URL "https://github.com" and target host is "github.com"
- **THEN** the item matches

#### Scenario: Subdomain mismatch
- **WHEN** item has URL "https://api.github.com" and target host is "github.com"
- **THEN** the item does NOT match

### Requirement: Title matching
The system SHALL match items by comparing the target title as a case-insensitive substring of the item's title.

#### Scenario: Substring match
- **WHEN** item title is "GitHub Personal" and target title is "github"
- **THEN** the item matches

#### Scenario: No match
- **WHEN** item title is "GitLab" and target title is "github"
- **THEN** the item does NOT match

### Requirement: Combined search
`auth-source-op--search-items` SHALL accept host and optional title parameters. Items SHALL match if they match host OR title. Results are drawn from the item cache.

#### Scenario: Match by host only
- **WHEN** an item matches by host but not title
- **THEN** the item is included in results

#### Scenario: Match by title only
- **WHEN** an item matches by title but not host
- **THEN** the item is included in results

#### Scenario: No matches
- **WHEN** no items match either host or title
- **THEN** an empty list is returned
