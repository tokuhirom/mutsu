---
name: roast-triage
description: Choose which roast work to do next and investigate one failing roast test — PLAN.md/BLOCKERS.md-driven task selection, the raku-vs-mutsu comparison order, and the roast-history.sh diagnostic categories. Use when picking the next roast target or debugging a specific failing roast/*.t file.
metadata:
  short-description: Pick and investigate roast work
---

# Roast triage

Two separate questions, in this order: *which* roast work to do, and *why* one specific file
fails.

## Choosing what to work on

**Primary: PLAN.md → BLOCKERS.md → then individual tests.** The project is in its final
stretch; work is driven by strategic priorities, not random test selection.

1. **PLAN.md priorities first.** Check the current quarter's section for high-impact tasks
   (exception types, performance, module compatibility, ...). They are chosen because they
   unblock many tests or advance project goals.
2. **`TODO_roast/BLOCKERS.md` for roast work.** It is the single ledger of all non-whitelisted
   roast tests, tracked per file and by root cause, with a raku-baseline column, and it groups
   failing tests by the missing feature. Implement the features that unblock the most tests
   (e.g. "Exception Types" blocks 22 tests, "Threading" blocks 31).
3. **Then individual tests**, for features already in progress.

**Do NOT cherry-pick easy tests to game the pass count.** The goal is implementing missing
features with broad impact — and do not skip a task because it looks hard (see "Working on
complex features" in `CLAUDE.md`: a test needing several unrelated features gets them all, in
one PR if that is what it takes).

When you defer a test, record *why* in its `TODO_roast/BLOCKERS.md` row (or the "Investigation
notes" section for longer findings). When a test reaches the whitelist, remove its row — the
details move to `news/`.

### Diagnostic tools (status tracking, not task selection)

`./scripts/roast-history.sh` generates per-file category lists under `tmp/`:

| File | Meaning |
| --- | --- |
| `tmp/roast-panic.txt` | Rust panics — highest priority to fix |
| `tmp/roast-timeout.txt` | timeouts |
| `tmp/roast-error.txt` | no valid TAP plan |
| `tmp/roast-fail.txt` | some subtests failing |
| `tmp/roast-pass.txt` | fully passing |

Re-run it after a change to find newly passing tests.

## Investigating one failing test

Before writing any code, always investigate in this order:

1. **Run it with `raku`** to see the expected output: `raku <roast-test-path>`.
   (No `raku` on this machine? Use the `install-raku` skill — do not work without the oracle.)
2. **Dump the AST with `raku`** if needed: `raku --target=ast -e '<relevant code>'`.
3. **Dump the AST with mutsu**: `timeout 30 target/debug/mutsu --dump-ast <roast-test-path>`.
4. **Run it with mutsu**: `timeout 30 target/debug/mutsu <roast-test-path>`.
5. Compare the outputs to identify what mutsu is doing wrong, then fix the interpreter.

Run a single roast test with fudge preprocessing enabled — **`MUTSU_FUDGE=1` is required**, or
`#?rakudo skip/todo` directives are ignored and the counts come out wrong:

```bash
cargo build && MUTSU_FUDGE=1 prove -e 'target/debug/mutsu' roast/<path>.t
```

Never set `MUTSU_FUDGE` when running ordinary (non-roast) scripts — a stray `#?rakudo skip`
comment would drop the next statement.

## Rules that constrain any fix

- `roast/` is **read-only** — a vendored upstream copy pinned in `vendor.lock`, updated only via
  `scripts/update-vendor.sh`. Roast is the authoritative spec: if passing a roast test requires
  changing a local `t/` test, change the `t/` test.
- **Never add special-case logic, hardcoded results, or test-specific hacks** to pass a test.
  Every fix must be a genuine, general-purpose improvement.
- Do not add a test to `roast-whitelist.txt` unless `prove -e 'target/debug/mutsu' <file>` exits
  cleanly, and keep the file sorted (`LC_ALL=C sort -c roast-whitelist.txt`) — CI fails if it is
  not.
- **Never remove a previously passing test from the whitelist** because of a regression; fix the
  regression. When `make roast` shows failures in whitelisted tests, investigate each one — do
  not dismiss them as "pre-existing" (check "Known flaky tests" in `CLAUDE.md` first, and follow
  its triage protocol before trusting any "flaky" label).
