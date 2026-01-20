# auth-source-op

Emacs auth-source backend for 1Password via the `op` CLI.

## AI-Generated Code Notice

This code was generated entirely by Claude (Anthropic) with human direction and
review. The copyright status of LLM-generated code is legally uncertain.

## Overview

This package enables `auth-source-search` to retrieve credentials from
1Password, allowing packages like Gnus, Forge, TRAMP, and others to
authenticate transparently using your 1Password vault.

## Requirements

- Emacs 28.1 or later
- [1Password CLI](https://developer.1password.com/docs/cli/) (`op`) installed
  and in your PATH
- 1Password desktop app (for biometric unlock)

## Installation

### From source

Clone the repository and add it to your load path:

```elisp
(add-to-list 'load-path "/path/to/emacs-auth-source-op")
(require 'auth-source-op)
(auth-source-op-enable)
```

### With use-package

```elisp
(use-package auth-source-op
  :load-path "/path/to/emacs-auth-source-op"
  :config
  (auth-source-op-enable))
```

## Configuration

### Basic setup

After installation, enable the backend:

```elisp
(auth-source-op-enable)
```

This adds `'1password` to the front of `auth-sources`, so 1Password is checked
before other backends (like `~/.authinfo`).

### Biometric retry count

By default, the package retries biometric authentication 3 times before giving
up. You can customize this:

```elisp
(setq auth-source-op-retry-count 5)
```

## Usage

Once enabled, 1Password works automatically with any package that uses
`auth-source-search`. The backend matches credentials by comparing the requested
host against:

1. **URL hostnames** in your 1Password items (strict matching, no wildcards)
2. **Item titles** (case-insensitive substring matching)

### Example: Forge (GitHub)

If you have a 1Password item with:
- Title: "GitHub"
- URL: `https://github.com`
- Username: `your-username`
- Password: `your-token`

Forge will automatically find it when authenticating to `api.github.com`.

### Example: SMTP/Email

For email credentials, ensure your 1Password item has:
- A URL matching your SMTP server (e.g., `https://smtp.gmail.com`)
- Username and password fields

### Multiple matches

When multiple 1Password items match a host, you'll be prompted to select one
using `completing-read`.

## Commands

| Command                        | Description                              |
| ------------------------------ | ---------------------------------------- |
| `auth-source-op-enable`        | Enable 1Password as an auth-source       |
| `auth-source-op-refresh-cache` | Force refresh of the item cache          |
| `auth-source-op-cache-list`    | Display cached items (secrets redacted)  |
| `auth-source-op-cache-clear`   | Clear all cached data                    |

## How it works

1. When a package calls `auth-source-search`, the 1Password backend is invoked
2. The backend searches a local cache of item metadata (titles, URLs, IDs)
3. Matching items are fetched from 1Password to retrieve credentials
4. Secrets are returned as closures (deferred retrieval) to minimize exposure

### Caching

Item metadata is cached after the first search to avoid repeated `op` CLI calls.
The cache stores item summaries (titles, URLs, timestamps) but never secrets.

- Use `auth-source-op-refresh-cache` to update the cache manually
- Use `auth-source-op-cache-clear` to clear all cached data
- Use `auth-source-op-cache-list` to view cached items

### Security considerations

- Secrets are never stored in the cache
- Secrets are retrieved only when actually needed (lazy evaluation)
- The `:secret` value is a closure, not a plain string
- `auth-source-op-cache-list` never displays secrets

## Troubleshooting

### "1Password CLI `op' not found in PATH"

Install the 1Password CLI from
https://developer.1password.com/docs/cli/

Ensure it's in your PATH:

```bash
which op
```

### Biometric authentication keeps failing

1. Ensure the 1Password desktop app is running
2. Enable CLI integration in 1Password settings
3. Try increasing `auth-source-op-retry-count`

### No credentials found

1. Run `auth-source-op-refresh-cache` to update the cache
2. Run `auth-source-op-cache-list` to verify items are cached
3. Check that your 1Password item has a URL matching the requested host
4. Remember: hostname matching is strict (no wildcards, no subdomain inference)

### Falling back to other backends

If 1Password lookup fails (CLI error, user cancellation, no match), the package
returns nil, allowing auth-source to fall back to other backends like
`~/.authinfo`.

## License

This project is dual-licensed. You may choose either:

- **The Unlicense** (public domain dedication)
- **GPL-3.0-or-later** (for compatibility with Emacs ecosystem)

See [LICENSE](LICENSE) for details.
