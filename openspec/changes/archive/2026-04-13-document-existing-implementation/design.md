## Context

auth-source-op is a complete, working Emacs package that bridges `auth-source-search` to 1Password via the `op` CLI. It has no formal specs. This change documents the existing implementation to establish a baseline.

## Goals / Non-Goals

**Goals:**
- Document every public and internal capability as testable specs
- Capture current behaviour accurately, including edge cases

**Non-Goals:**
- Changing any existing behaviour
- Adding new features or refactoring code
- Specifying internal implementation details that don't affect observable behaviour

## Decisions

**Spec granularity**: Six specs matching the natural module boundaries in the code (CLI interface, cache, field mapping, search, disambiguation, backend). Each spec is self-contained. Rationale: mirrors the `;;; Section` comments in the source, making it easy to find relevant specs from code and vice versa.

**Document observable behaviour only**: Specs describe what callers see (return values, side effects, prompts) not internal data structures. Rationale: allows refactoring without spec churn.

## Risks / Trade-offs

- [Specs drift from code again] → Mitigated by openspec's structured change workflow; future changes require spec updates before implementation.
- [Over-specifying internals] → Kept specs at the behavioural level; internal helpers are not individually specified.
