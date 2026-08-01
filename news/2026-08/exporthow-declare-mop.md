# EXPORTHOW::DECLARE + HOW-driven class registration — OO::Monitors runs verbatim

The `monitor` declarator was briefly provided natively (#5640) as an
explicitly temporary stopgap; native provision of ecosystem modules is
banned (CLAUDE.md working agreements). This campaign built the machinery to
run the real, vendored `OO::Monitors` verbatim, and retired the stopgap.

## What landed

1. **`EXPORTHOW::DECLARE::<keyword>` — a NEW declarator from a module.**
   The parse-time module scan detects
   `my package EXPORTHOW { package DECLARE { constant kw = SomeHOW } }` and
   registers the keyword for the importing unit; a generic `declare_decl`
   parser (generalizing the previously hardcoded `monitor_decl`) parses
   `kw Name { ... }` like a class and tags the ClassDecl with the keyword.
   The table is unit-scoped (cleared on parser reset, restored wholesale
   around nested module scans), and the parser's scan search path now
   includes the bundled battery paths (`parser_scan_lib_paths`).

2. **Class registration drives the user HOW protocol**
   (`declare_drive_how_protocol`): `new_type` (its `callsame` resolves to
   the registered type object; OO::Monitors' `setup_monitor` adds the lock
   attribute through the native `add_attribute` bridge), `add_method` per
   declared method (the override `.wrap`s the Method object — landing in
   `method_wrap_chains` via its owner markers — and re-adds it through the
   fully-qualified native `self.Metamodel::ClassHOW::add_method`, a no-op
   for an already-registered method), and the queued user `compose` (reads
   `self.method_table`, installs BUILDALL/POPULATE and `clone` via
   `anon method`). New native bridges: fully-qualified
   `self.Metamodel::ClassHOW::<meth>` dispatch, callsame base candidates
   for every native ClassHOW metamethod, a user BUILDALL/POPULATE running
   at construction (`run_user_buildall_hook`) with `callsame` resolving to
   the built instance, and a user `clone`'s `callsame` reaching the native
   attribute-copying clone.

3. **General bugs fixed along the way** (each surfaced by running the real
   module): LEAVE/ENTER phasers inside an `if`/`else` branch never fired,
   in statement or value position (OO::Monitors unlocks in a LEAVE inside
   `if SELF.DEFINITE { }` — the lock stayed held and cross-thread calls
   deadlocked); the slow-path `call_compiled_method` dropped an added
   method's captured creating-scope env; `^add_method` lost a NAMED
   invocant binding (`anon method (Mu \SELF: |)`); `Attribute.new`'s
   get_value/set_value used the sigiled name as the storage key (every
   other Attribute builder uses the bare name) and silently no-opped on a
   Scalar-wrapped object; `install_custom_class_how` read the lazily-filled
   `ClassDef::mro` directly, so a module-loaded HOW's user `compose` was
   never detected.

4. **Stopgap retired.** The `MONITOR_DECL_ENABLED` parser gate, the
   `__mutsu_monitor` marker trait, the monitor-lock hooks in
   `call_compiled_method(_fast)`, the process-global monitor registry, and
   the `OO::Monitors` native-module arm are all removed. The module is
   vendored at `modules/OO-Monitors/` and `batteries.lock` points at the
   bundled lib.

## Verification

The upstream suite (5 files, 9 subtests, pinned at `5f3af495`) passes
verbatim, including the 4-thread increment serialization test.
`t/oo-monitors-battery.t` pins the observable behavior;
`t/exporthow-declare-keyword.t` and `t/exporthow-declare-monitor-protocol.t`
pin the DECLARE mechanism and the HOW protocol; `t/leave-in-if-branch.t`
pins the phaser fix.

This also reifies mutsu's class registration as an overridable protocol —
the enabling step for other Metamodel-based ecosystem modules.
