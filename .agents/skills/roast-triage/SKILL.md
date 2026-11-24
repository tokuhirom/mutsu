---
name: roast-triage
description: Investigate one failing roast test — the raku-vs-mutsu comparison order, the roast-history.sh diagnostic categories, and where a non-whitelisted file's reason is recorded. Use when a whitelisted roast/*.t regresses, or when looking at one of the remaining non-whitelisted files.
metadata:
  short-description: Investigate a failing roast test
---

# Roast triage

Roast's role today, and how to find *why* one specific file fails.

## Roast is a regression guard

Roast is no longer a work source: 1440 of the 1455 `.t` files are whitelisted, and every one of
the remaining 15 is a non-goal, has no oracle, or cannot pass. The table in
`docs/roast-compatibility.md` ("Non-whitelisted roast files and why") gives the reason for each.
New work comes from `PLAN.md`, the issue queue by lane and the ecosystem (see "Agents and queue
requests" in `AGENTS.md`). This skill is for the two cases where roast still needs attention:

1. **A whitelisted file fails.** That is a regression: investigate it (below) and fix it. Never
   remove the file from `roast-whitelist.txt` because of it.
2. **One of the non-whitelisted files.** Re-measure before trusting its row. If general work
   moved it, update the row; if it now passes, whitelist it and remove the row. A file whose
   whitelist entry a roast re-vendor broke for a reason that is not a mutsu bug gets a new row
   there, with the reason.

**Do NOT cherry-pick easy tests to game the pass count**, and do not add test-specific hacks to
whitelist a non-goal file.

### Diagnostic tools (status tracking)

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
  not dismiss them as "pre-existing" (check "Known flaky tests" in `AGENTS.md` first, and follow
  its triage protocol before trusting any "flaky" label).
