# Battery: monitors (serialized objects) — `OO::Monitors`

**Slot:** Monitors (`monitor` declarator) · **Chosen:** `OO::Monitors`
(upstream `zef:raku-community-modules` / jnthn, v1.1.7, Artistic-2.0) ·
**Kind:** Bundled, runs verbatim (`modules/OO-Monitors/`)

## What it is

`OO::Monitors` provides the `monitor` declarator: a class whose method calls
are mutually excluded per instance — at most one thread runs ANY method on a
given instance at a time, with a reentrant lock so a method can call its
siblings:

```raku
use OO::Monitors;

monitor Counter {
    has $!count = 0;
    method inc() { $!count++ }        # safe from many threads at once
    method current() { $!count }
}
```

**It is a hard dependency of Cro::HTTP** (connection state, session stores —
Cro declares most of its shared-state classes as monitors).

## How the verbatim module runs under mutsu

The upstream implementation is ~110 lines of Metamodel machinery, and mutsu
executes all of it (the EXPORTHOW::DECLARE campaign; it was briefly provided
natively, #5640, until the machinery below landed):

- **`EXPORTHOW::DECLARE` keyword registration** — the parse-time module scan
  detects `my package EXPORTHOW { package DECLARE { constant monitor =
  MetamodelX::MonitorHOW } }` and registers `monitor` as a class-like
  declarator for the importing unit (`module_exports.rs` →
  `declare_decl` in `stmt/class/class_decl.rs`). The keyword table is
  unit-scoped and restored around nested module scans; bundled battery
  paths are part of the parser's scan search path
  (`parser_scan_lib_paths`).
- **HOW-driven registration** — a `monitor`-declared class registers
  natively, then drives the user HOW protocol
  (`declare_drive_how_protocol`, `runtime/metamodel.rs`): `new_type` (its
  `callsame` resolves to the registered type; `setup_monitor` adds the
  `$!MONITR-lock` attribute through the native `add_attribute` bridge),
  `add_method` per declared method (the override `.wrap`s each Method
  object — landing in `method_wrap_chains` — and re-adds it via the
  fully-qualified `self.Metamodel::ClassHOW::add_method`, a no-op for an
  already-registered method), then the queued user `compose` (reads
  `self.method_table`, installs a BUILDALL/POPULATE pair and a `clone`
  via `anon method`).
- **Construction & callsame bridges** — a user BUILDALL/POPULATE runs at
  `.new`/`bless` (`run_user_buildall_hook`) so the lock attribute is
  seeded before any method call; `callsame` from BUILDALL/POPULATE/clone
  resolves to the native base behavior
  (`native_mu_base_next_candidate`), and `callsame` from any HOW method
  falls through to the native ClassHOW metamethods.
- **Locking** — entirely the module's own wraps: each method call acquires
  the per-instance `Lock` (reentrant in mutsu, so sibling calls don't
  deadlock) and releases it in a `LEAVE` inside `if SELF.DEFINITE { ... }`
  (block-scoped LEAVE in an `if` branch fires correctly — fixed as part of
  this campaign).

Upstream tests: 5 files, 9 subtests — all pass under mutsu, matching raku,
including the 4-threads × 1000-increments serialization test. Smoke:
`t/oo-monitors-battery.t` (adds a reentrancy case).

## Provenance and update procedure

| Module | Upstream | Pinned version | Commit |
| --- | --- | --- | --- |
| `OO::Monitors` | <https://github.com/jnthn/oo-monitors> | v1.1.7 | `5f3af495` (2025-11-05) |

The library is vendored verbatim at `modules/OO-Monitors/` (lib + LICENSE +
META6.json + README). The upstream suite is fetched at the pinned commit by
the release gate (`scripts/battery-testsuite.sh`) and runs against the
bundled lib. To bump: re-copy the upstream `lib/` tree, update the `commit`
in `batteries.lock`, and re-run `scripts/battery-testsuite.sh --update`.

Verification after a bump:

```sh
mutsu -e 'use OO::Monitors; monitor M { method hi { "hi" } }; say M.new.hi'   # hi
```

## License

Upstream `OO::Monitors` is **Artistic-2.0**; the license text is vendored at
`modules/OO-Monitors/LICENSE` alongside the redistributed source.
