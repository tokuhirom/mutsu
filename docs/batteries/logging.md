# Battery: general-purpose logging — `Log::Async`

**Slot:** Structured application logging (debug/info/warn/error, distinct from
`Log::Timeline`'s task/event-timeline instrumentation) · **Selected:**
`Log::Async` v0.0.17 (`auth<zef:bduggan>`, Artistic-1.0-Perl) · **Kind:**
Selected, not yet bundled (blocked on a parser gap in its own dependency) ·
**Yardstick:** [BATTERIES.md §2](../../BATTERIES.md#2-selection-criteria) —
license (hard gate) → dependency weight → maintainer stewardship → proven
behaviour on mutsu → API fit → "a small web blog can be written with the
bundle alone"

Procedure: [selection-method.md](selection-method.md).

## Status: selected, not yet bundled

`Log::Async` won the field on ecosystem standing and API simplicity, but its
only runtime dependency, `Terminal::ANSI`, ships one file written in a parser
form mutsu does not yet support (`unit monitor ...;`) — see
[Why it currently fails on mutsu](#why-it-currently-fails-on-mutsu) below.

```raku
use Log::Async <trace>;
trace 'starting up';
error 'connection refused';   # not yet runnable on mutsu
```

## What it is not: `Log::Timeline`

mutsu already bundles `Log::Timeline` (`modules/Log-Timeline/`, see
[oo-monitors.md](oo-monitors.md) for its `monitor`-declarator dependency and
[cro-deps.md](cro-deps.md) for its own status), but that module is a distinct
tool for a distinct job: it records **tasks with start/end periods and
phases** for visualizing overlapping work over time (its own README: "Log
tasks with start and end periods and phases, as well as individual events"),
consumed by tools like [Comma](https://commaide.com/) and Cro's request
pipeline. It has no `debug`/`info`/`warn`/`error`-style plain-text logging
API and outputs only structured JSON-lines/CBOR/socket streams. This slot is
for the missing conventional line-oriented application logger; it does not
replace or compete with `Log::Timeline`.

## The field

Enumerated from `~/.zef/store/rea/rea.json` (~14,834 dist names), filtered on
`log`/`logger`/`logging` in name/description/tags.

| Candidate | Version | Released | License | auth / GitHub owner | Runtime deps | Dependents¹ | raku | mutsu |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| **`Log::Async`** | 0.0.17 | 2026-03-09 | Artistic-1.0-Perl | `zef:bduggan` ([bduggan](https://github.com/bduggan/raku-log-async), ★12) | 1 (`Terminal::ANSI`) | **17** (highest — `AI::Gator`, `Jupyter::Kernel`, `Curlie`, ...) | loads cleanly | **fails to load** (dependency parser gap) |
| `LogP6` | 1.6.4 | 2021-02-23 | Artistic-2.0 | `cpan:ATROXAPER` ([atroxaper](https://github.com/atroxaper/p6-LogP6), ★8) | 2 (`UUID`, `JSON::Fast` — both already bundled) | 3 | loads cleanly | **fails to load** (own-source parser gap) |
| `Log::Timeline` | 0.5.2 | 2024-11-30 | Artistic-2.0 | `zef:raku-community-modules` ([repo](https://github.com/raku-community-modules/Log-Timeline), ★4) | 2 (`CBOR::Simple`, `JSON::Fast`) | 5 | — | already bundled, different slot (see above) |
| `Log::Dispatch` | 0.0.8 | 2022-11-05 | Artistic-2.0 | `zef:vrurg` ([vrurg](https://github.com/vrurg/raku-Log-Dispatch), ★0) | 1 (`Terminal::ANSI`) | 0 | loads cleanly | **fails to load** (two separate bugs, see below) |
| `Log::Syslog::Native` | 0.1.2 | 2024-10-02 | Artistic-2.0 | ([jonathanstowe](https://github.com/jonathanstowe/Log-Syslog-Native), ★1) | 0 | 3 | — | not evaluated (POSIX-syslog-only, out of scope for a general logger) |
| `Log::Any` | 0.9.5 | 2018-11-25 (last commit 2019-03) | Artistic-2.0 | `github:jsimonet` ([repo](https://github.com/jsimonet/log-any), ★2) | unknown | 1 | — | not evaluated (effectively abandoned) |

¹ Distributions in the local REA index whose `depends` names the candidate,
same methodology as [templates.md](templates.md) / [toml.md](toml.md).

## Ruled out before measuring

- **`Log::Simple`, `Log::Colored`, `Log::JSON`** (all `cpan:TYIL`, all
  depending on the same author's base `Log` dist, all last released 2020) —
  **AGPL-3.0-only / LGPL-3.0**. Copyleft; disqualified by the hard license
  gate ([BATTERIES.md §4](../../BATTERIES.md#4-license-policy)) before any
  other criterion was checked. Zero dependents each, so nothing else was lost
  by excluding them.
- **`Log::Syslog::Native`** — not a general application logger, a thin
  NativeCall binding to POSIX `syslog(3)`. Out of scope for "the general
  logging slot"; worth a look later as a companion sink once a general logger
  is bundled (`Log::Async` itself supports pluggable sinks via `send-to`).
- **`Log::Any`** — last real commit 2019-03, no meaningful ecosystem uptake
  (1 dependent). Age alone is a strong enough signal per
  [selection-method.md](selection-method.md#2-compute-the-metrics) to skip a
  full measurement pass.

## Why `Log::Async` was passed over... it wasn't — but the runner-up matters

`Log::Async` wins decisively on the dependents count (17, more than 3x the
next real contender) and by far the most active maintenance (released
2026-03-09, versus the entire rest of the field last touched 2021-2024). Its
API is also the simplest in the field:

```raku
use Log::Async <trace>;
trace 'connecting to database';
error 'connection refused: ', $reason;
```

The one wrinkle worth recording explicitly (per
[selection-method.md](selection-method.md#2-compute-the-metrics)'s "always
state the release date and any oddity in the final decision, not just the
metrics table"): its license is **Artistic-1.0-Perl**, not the 2.0 that most
of this bundle's other entries carry. Artistic-1.0 is still OSI-approved and
permissive; it is not a copyleft license and does not trip the hard gate in
[BATTERIES.md §4](../../BATTERIES.md#4-license-policy). Noted here so a
future maintainer does not have to re-derive it.

`LogP6` is the most credible runner-up if `Log::Async` were ever disqualified:
richer feature set (named loggers via `get-logger('category')`, pluggable
`Writer`s including a separate `journald` package, async I/O handles), and
both its runtime dependencies (`UUID`, `JSON::Fast`) are already bundled — so
it would add zero new dependency surface. It loses on ecosystem standing (3
dependents vs. 17) and has had no commits since 2021-02-23. Measured against
mutsu as part of this survey: `LogP6` fails to even load. Its own source
(`lib/LogP6.rakumod`, `create-and-store-loggers`) uses the idiom
`(%cliches-to-traits{$cliche.name} //= SetHash.new){$trait} = True;` — a
parenthesized hash-subscript compound-assignment immediately followed by a
chained postcircumfix `{}` — which mutsu's parser cannot parse at all (a hard
`Confused. expected statement` at statement level, or a silent mis-parse as
two separate statements as a call argument). This is a general parser gap,
not `LogP6`-specific — filed as
[paren-subscript-compound-assign-then-postcircumfix-fails-to-parse.md](../../todo/tickets/paren-subscript-compound-assign-then-postcircumfix-fails-to-parse.md).

`Log::Dispatch` was not seriously considered as the winner: zero dependents
and zero GitHub stars indicate no real ecosystem uptake, despite a reasonable
API (`Log::Dispatch::Destination` role for custom sinks). Measured against
mutsu: it also fails to load, on its very first module-level statement, `my
Lock:D $reg-lock .= new;` — mutsu's `my`-declaration `.=`-initializer path
does not strip the `:D` definedness smiley from the type constraint before
using it as the method-call target, so it tries to dispatch `.new` against
the literal type name `Lock:D` and fails
(`X::Method::NotFound: ... new on Lock:D`). This is unrelated to logging
specifically — it reproduces for any `my Type:D $x .= new;` declaration —
filed as
[lexical-typed-var-dot-equals-init-fails.md](../../todo/tickets/lexical-typed-var-dot-equals-init-fails.md).
Working around that still leaves `Log::Dispatch` blocked on the same
`Terminal::ANSI` / `unit monitor` gap as `Log::Async`
([unit-monitor-declarator-not-supported.md](../../todo/tickets/unit-monitor-declarator-not-supported.md)).

Neither of these two bugs affects `Log::Async` itself or its own dependency
chain — both are recorded as general interpreter gaps found as a side effect
of evaluating the runner-up candidates, not as blockers for the chosen
module.

## Why it currently fails on mutsu

`Log::Async` itself loads cleanly — the failure is entirely in its one
runtime dependency, `Terminal::ANSI`. That distribution ships
`Terminal::ANSI::Virtual.rakumod` declared as:

```raku
unit monitor Terminal::ANSI::Virtual;
```

the **file-scope (`unit`) form** of the `monitor` declarator that
`OO::Monitors` registers (mutsu already bundles `OO::Monitors`, see
[oo-monitors.md](oo-monitors.md)). mutsu supports the **block** form
(`monitor Foo { ... }`) but not this file-scope form, so the load fails with:

```
Unknown function: monitor
```

This is a **general parser gap** (any `EXPORTHOW::DECLARE`-registered
keyword used in `unit` form hits it, not specific to `Terminal::ANSI` or
`Log::Async`), fully root-caused and scoped as a small, self-contained fix —
see
[todo/tickets/unit-monitor-declarator-not-supported.md](../../todo/tickets/unit-monitor-declarator-not-supported.md).
Per [BATTERIES.md §1](../../BATTERIES.md#1-adoption-policy--community-first-adopt-as-is)
rung 2, the answer is to grow mutsu's parser, not patch the vendored module.

## Provenance (for when the blocker clears)

Per [BATTERIES.md §3](../../BATTERIES.md#3-vendoring-and-resolution). Not yet
vendored — no `modules/` tree or `batteries.lock` entry exists for this slot
yet, per the "Selected, not yet bundled" convention in
[BATTERIES.md §7](../../BATTERIES.md#7-bundle-index).

| Module | Upstream | Pinned version | auth |
| --- | --- | --- | --- |
| `Log::Async` | <https://github.com/bduggan/raku-log-async> | v0.0.17 | `zef:bduggan` |
| `Terminal::ANSI` (dependency) | <https://github.com/bduggan/raku-terminal-ansi> | latest at bundle time | (MIT; depends only on the already-bundled `OO::Monitors`) |

When vendoring: `lib/` + `META6.json` + `LICENSE` + `README.md` for both
modules, excluding upstream `t/`/CI config, per the standard recipe. The
bundling work itself (once the parser blocker clears) is tracked as
[todo/tickets/bundle-log-async-battery.md](../../todo/tickets/bundle-log-async-battery.md).

## Security updates

Per [BATTERIES.md §6](../../BATTERIES.md#6-security-updates-and-independent-updatability),
once bundled, the vendored copy is the lowest-priority source; `mzef install
Log::Async` shadows it without a mutsu release.

## License

**Artistic-1.0-Perl** for `Log::Async` (declared in `META6.json`) — permissive,
OSI-approved, not copyleft; passes
[BATTERIES.md §4](../../BATTERIES.md#4-license-policy) without needing a
provisional-exception caveat. `Terminal::ANSI` is MIT.

## Next steps

1. Land
   [todo/tickets/unit-monitor-declarator-not-supported.md](../../todo/tickets/unit-monitor-declarator-not-supported.md)
   (the `unit monitor` parser gap).
2. Re-verify `use Log::Async; trace 'x';` loads and runs under mutsu.
3. Vendor `Log::Async` + `Terminal::ANSI` per
   [BATTERIES.md §3](../../BATTERIES.md#3-vendoring-and-resolution) — tracked
   in
   [todo/tickets/bundle-log-async-battery.md](../../todo/tickets/bundle-log-async-battery.md).
4. Add the `batteries.lock` entries, run
   `scripts/battery-testsuite.sh --update`, and promote this record's status
   line + the [bundle index](../../BATTERIES.md#7-bundle-index) row from
   "Selected, not yet bundled" to "Working".
