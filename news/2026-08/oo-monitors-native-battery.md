# OO::Monitors provided natively — the `monitor` declarator works

`use OO::Monitors` now enables the `monitor` declarator, natively: a monitor
is a class whose instance-method calls serialize on a per-instance REENTRANT
lock. All 5 upstream test files (9 subtests) pass, matching raku — including
the 4-threads × 1000-increments race, which lands on exactly 4000. It is a
hard dependency of Cro::HTTP (most of Cro's shared-state classes are
monitors) — the fifth Cro::HTTP dependency locked in behind the release gate.

Native (rung 3) rather than vendored (rung 2), deliberately: the upstream
implementation is ~110 lines of Rakudo Metamodel guts — a
`Metamodel::ClassHOW` subclass installed via `EXPORTHOW::DECLARE`,
`.wrap`/`callsame` on every method, raw Attribute cell pokes — the same
NQP/metamodel surface that put Test::Async out of reach. mutsu already
provides Test / JSON::Fast / NativeCall natively on the same reasoning; the
selection record (`docs/batteries/oo-monitors.md`) spells out the exit path
(retire the native declarator if real EXPORTHOW support ever lands).

Implementation notes:

- The parser recognizes `monitor` only after `use OO::Monitors` (a
  unit-scoped flag; the module export scan saves/restores it across nested
  parses). The declaration parses exactly like `class` plus a
  `__mutsu_monitor` marker trait.
- Registration records the class in a process-global monitor set; the
  common no-monitor program pays one atomic load per method dispatch.
- Both compiled-method chokepoints (`call_compiled_method` /
  `call_compiled_method_fast`) take the per-instance lock with the same
  critical-section bracketing as `Lock.protect`, so attribute mutations
  commit across threads. Construction/clone plumbing is exempt (matching
  upstream's wrap exclusions), type-object calls never lock, and subclasses
  of a monitor stay serialized. mutsu's Lock runtime is already reentrant
  (owner + recursion count), so sibling-method calls cannot self-deadlock.
- The release gate learned a `-` `bundled_lib` in `batteries.lock`: a
  natively-provided battery runs its upstream suite with no `-I`.

Packaging: `batteries.lock` row (tests-only, native), all 5 files in
`batteries-whitelist.txt`, `t/oo-monitors-battery.t` smoke test (adds a
reentrancy case), the selection record, and the BATTERIES.md §7 index row.
