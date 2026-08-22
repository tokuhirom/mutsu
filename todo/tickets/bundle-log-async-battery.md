# Vendor `Log::Async` + `Terminal::ANSI` as the general logging battery

## What

`Log::Async` (`auth<zef:bduggan>`, v0.0.17, Artistic-1.0-Perl) won the survey
for mutsu's missing general-purpose (debug/info/warn/error) logging battery
slot over `LogP6`, `Log::Dispatch`, `Log::Syslog::Native`, and `Log::Any` —
see the full survey and metrics table in
[docs/batteries/logging.md](../../docs/batteries/logging.md). It wins
decisively on ecosystem standing (17 dependents, more than 3x the next
candidate) and maintenance activity (last released 2026-03-09, vs.
2021-2024 for the rest of the field).

This ticket is the actual vendoring work, split out from the survey/selection
record per [selection-method.md](../../docs/batteries/selection-method.md)'s
"two files come out of a survey" convention: the decision is written down in
`docs/batteries/logging.md`, this ticket is the follow-up execution.

## Blocked on

[todo/tickets/unit-monitor-declarator-not-supported.md](unit-monitor-declarator-not-supported.md) —
`Log::Async`'s only runtime dependency, `Terminal::ANSI`, ships
`Terminal::ANSI::Virtual.rakumod` as `unit monitor Terminal::ANSI::Virtual;`,
the file-scope form of the `monitor` declarator, which mutsu's parser does
not yet accept (`Unknown function: monitor`). `Log::Async` itself loads
cleanly under mutsu; only this one file in its dependency chain is affected.
**Do not start this ticket until that one is fixed and re-verified.**

## Steps (once unblocked)

Follow [BATTERIES.md §3](../../BATTERIES.md#3-vendoring-and-resolution), the
same recipe already used for `UUID`/`OO::Monitors`/etc:

1. Re-verify the blocker is actually clear:
   ```sh
   mutsu -e 'use Log::Async <trace>; trace "hello"'
   ```
2. Fetch `Log::Async` and `Terminal::ANSI` at their pinned commits (see
   `docs/batteries/logging.md`'s Provenance table for upstream URLs).
3. Vendor `lib/` + `META6.json` + `LICENSE` + `README.md` for both into
   `modules/Log-Async/` and `modules/Terminal-ANSI/` (excluding upstream
   `t/`/CI config — the release gate fetches tests fresh at the pinned
   commit, per the standard recipe `docs/batteries/uuid.md` documents).
4. Add both to `batteries.lock`.
5. Run `scripts/battery-testsuite.sh --update` and confirm the upstream test
   suites for both dists pass (or gate/waive individual files with a reason,
   same as every other bundled battery).
6. Refresh the Pages manifest: `python3 scripts/gen-batteries-manifest.py`.
7. Update `docs/batteries/logging.md`'s status line and the
   [BATTERIES.md §7 bundle index](../../BATTERIES.md#7-bundle-index) row from
   "Selected, not yet bundled" to "Working".
8. Add a smoke test, e.g. `t/log-async-battery.t`, exercising at least one
   call per log level and a custom `send-to` sink.

## Why this is its own ticket, not folded into the parser-gap ticket

The parser fix and the vendoring work are independent units of effort with
different skill shapes (interpreter internals vs. the mechanical
fetch/vendor/gate/document recipe) and should land as separate PRs, matching
how every other bundled battery in this repo was split from its own blocking
core-work ticket (e.g. `Config::TOML`'s
`token-method-bare-colon-adverb-name-not-supported.md` vs. its own eventual
bundling step, per `docs/batteries/toml.md`'s "Next steps").
