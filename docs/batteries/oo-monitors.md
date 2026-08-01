# Battery: monitors (serialized objects) — `OO::Monitors`

**Slot:** Monitors (`monitor` declarator) · **Chosen:** `OO::Monitors`
(upstream `zef:raku-community-modules` / jnthn, v1.1.7, Artistic-2.0) ·
**Kind:** Native (provided by the interpreter core)

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

## Why it is provided natively (not vendored)

**It is a hard dependency of Cro::HTTP** (connection state, session stores —
Cro declares most of its shared-state classes as monitors). The Cro campaign
(`docs/batteries/web-framework.md`) needs it working under mutsu.

**Why rung 3 (native) instead of rung 2 (run the real module):** the upstream
implementation is ~110 lines of Rakudo Metamodel guts — a
`Metamodel::ClassHOW` subclass installed via `EXPORTHOW::DECLARE` (a slang
hook), `.wrap`/`callsame` on every added method, raw `Attribute`
`get_value`/`set_value`, and NQPArray HLLization. That is the same
NQP/metamodel surface that put Test::Async out of reach
(`b2b-test-async-scouting`); implementing enough MOP to run it verbatim is a
multi-session campaign with no other consumer. mutsu already provides
`Test`, `JSON::Fast`, and `NativeCall` natively on the same reasoning, and
the *observable* monitor semantics are small and well-defined. The exit path
stands: if mutsu ever grows real `EXPORTHOW`/ClassHOW support, the native
declarator can be retired for the vendored module.

**How the native implementation works:**

- `use OO::Monitors` is a recognized native module (`runtime_module.rs`) and
  enables the `monitor` declarator in the parser for the rest of the unit
  (`use_decl.rs` → `monitor_decl` in `stmt/class/class_decl.rs`). The module
  export scan preserves the flag across nested parses.
- `monitor Foo { ... }` parses exactly like `class` and carries a
  `__mutsu_monitor` marker trait; registration records the class in a
  process-global monitor set (`vm_typedecl_ops.rs`,
  `native_methods/state_lock.rs`).
- Both compiled-method chokepoints (`call_compiled_method` /
  `call_compiled_method_fast` in `vm_method_dispatch.rs`) serialize calls on
  a per-instance REENTRANT lock (mutsu's `Lock` runtime), with the same
  critical-section bracketing as `Lock.protect` so attribute mutations
  commit across threads. Construction/clone plumbing (`new`, `bless`,
  `BUILDALL`, `POPULATE`, `clone`, `BUILD`, `TWEAK`) is exempt, matching the
  upstream wrap exclusions; type-object calls carry no instance and never
  lock. Subclasses of a monitor stay serialized (MRO check).

Upstream tests: 5 files, 9 subtests — all pass under mutsu, matching raku,
including the 4-threads × 1000-increments serialization test. Smoke:
`t/oo-monitors-battery.t` (adds a reentrancy case).

## Provenance and update procedure

The upstream suite is fetched at the pinned commit by the release gate — a
`bundled_lib` of `-` in `batteries.lock` marks a natively-provided battery
(no `-I`; the suite runs against the interpreter itself).

| Module | Upstream | Pinned version | Commit |
| --- | --- | --- | --- |
| `OO::Monitors` (tests only) | <https://github.com/jnthn/oo-monitors> | v1.1.7 | `5f3af495` (2025-11-05) |

To bump: update the `commit` in `batteries.lock` and re-run
`scripts/battery-testsuite.sh --update`; there is no vendored tree to
re-sync. Semantic changes upstream (new exclusions, new declarators) land as
interpreter changes with pins under `t/`.

Verification after a bump:

```sh
mutsu -e 'use OO::Monitors; monitor M { method hi { "hi" } }; say M.new.hi'   # hi
```

## License

Upstream `OO::Monitors` is **Artistic-2.0**; mutsu redistributes none of its
source (the implementation is original interpreter code), so no license text
is vendored. The record above credits the upstream design and pins its test
suite as the compatibility contract.
