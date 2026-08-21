# `scripts/check-value-wall.sh` false-positives on `Value::Hash` inside comments

`make test` currently fails locally at the `check-value-wall` step (before any
real test runs) on a clean `main` checkout (verified via `git stash` on an
unmodified working tree, commit `d28b9bc7e`):

```
value-wall ratchet FAILED: 2 direct Value:: variant uses outside src/value/ (baseline: 0).
```

The two matches are both in `src/vm/vm_hash_subclass_delegate.rs`, inside doc
comments (`//!`/`//`) that mention `Value::Hash` as prose, not as code:

```
src/vm/vm_hash_subclass_delegate.rs:13://! native coverage of its own), a plain `Value::Hash` already has full native
src/vm/vm_hash_subclass_delegate.rs:149:        // `Value::Hash` has no native `STORE` method of its own to delegate
```

`scripts/check-value-wall.sh`'s regex (`\bValue::(Int|...)\b`) does not
distinguish comments from code, so a prose reference trips the same ratchet as
a real violation. This is not a CI blocker (`.github/workflows/ci.yml` never
invokes `check-value-wall`), but it silently breaks the documented local
`make test` workflow for anyone who does not already know to skip that target.

Minimal fix: either reword the two comments to avoid the literal
`Value::Hash` token (e.g. "a plain Hash value" / backtick-free), or teach the
script to strip `//`/`//!` comment lines before grepping. Not fixed here —
out of scope for the change in progress (bundling the `UUID` battery); filed
so it doesn't evaporate.
