# The panic surface can no longer grow silently

[#8186](https://github.com/tokuhirom/mutsu/issues/8186) observed that `PLAN.md` §8.3 states mutsu
must never Rust-panic on any input, but nothing enforced it: the count of `unwrap`/`expect`/
`panic!`/`unreachable!` in `src/` rose at every architecture review taken so far (2,440 as of
2026-09-12), and `#[allow(` rose alongside it (243). A goal with no enforcement is a wish, and the
trend was the evidence.

## What shipped

Option 1 from the issue: a ratchet, on the same pattern as `opcode_size_guard`,
`scripts/check-value-wall.sh` and `scripts/check-magic-keys.sh`.

`scripts/check-panic-surface.py` counts `.unwrap()` / `.unwrap_err()` / `.expect(` / `panic!(` /
`unreachable!(` / `todo!(` / `unimplemented!(` and `#[allow(` occurrences across `src/`, and compares
the total against `scripts/panic-surface-baseline.txt`. Either count may go down (or stay flat);
raising it requires deliberately editing the baseline, so growth becomes something someone chose
instead of something nobody noticed.

Unlike the other ratchets, this one needed more than a line-grep: `#[cfg(test)]` items nest at
arbitrary depth in this codebase (not always a trailing `mod tests` block), and per the issue's own
note, test scaffolding should not consume the production budget. The script does a small
Rust-comment/string/brace scan — blanking `//`/`/* */` comments, `"..."` and raw `r#"..."#` strings,
and char literals, then blanking the balanced `{ ... }` body (or bare `...;` statement) of every
`#[cfg(test)]`-attributed item, stacked attributes included — before counting. That logic gets its
own `--self-test` against 17 synthetic snippets (comments, strings, nested `#[cfg(test)] mod`
inside a non-test `mod`, stacked `#[allow(]`+`#[cfg(test)]`, …), run by `make check-panic-surface`
before it trusts the count on the real tree — a masking bug could otherwise make the ratchet
silently report 0 no matter what `src/` contains.

Excluding test code drops the enforced baseline to **1,906** panic-family sites and **238**
`#[allow(` sites (from the raw ~2,475 / 243 a plain `grep -c` would count, test scaffolding
included).

Wired into `make test` (as `check-panic-surface`, alongside `check-value-wall`/`check-magic-keys`)
and as its own CI step in `test-check`, next to the other ledger checks — a `.rs`-only change like
this one still triggers the full test suite (`scripts/ci-docs-only.sh`'s allowlist excludes
`scripts/*.py`), so the check runs on every relevant PR from here.

`PLAN.md` §6 and §8.3 are updated to point at the ratchet instead of describing it as unenforced;
the remaining PLAN items (bring edge-case panics to zero, error-message quality) are the actual
shrinking work the ratchet now protects.

## Left open

The masker is a heuristic (attribute-ordering assumes `#[cfg(test)]` precedes any other stacked
attribute on the same item, the Rust convention here), not a full parser — acceptable for a ratchet
whose whole point is a stable, hard-to-game count, not a quality signal.
