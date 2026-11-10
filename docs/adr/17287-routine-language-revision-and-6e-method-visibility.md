# ADR-17287: A routine runs under its unit's language revision; the 6.e methods are process-wide

- **Status**: Accepted
- **Date**: 2026-11-10
- **Deciders**: Claude (ticket worker), tokuhirom (maintainer review on merge)
- **Issue**: [#17287](https://github.com/tokuhirom/mutsu/issues/17287)

## 1. Context

`use v6.e[.PREVIEW]` is declared per compilation unit, but the revision gates
(`REVISION_E` method rows, `revision_e()`, ~15 `current_language_version().starts_with("6.e")`
sites) read the parser's thread-local `CURRENT_LANGUAGE_VERSION`, which at run time holds the
revision of whichever unit was parsed last -- the main script. A sub or method of a module that
declared `use v6.*` was therefore judged by its *caller's* revision (ParaSeq's `Seq.snip` died
with `No such method 'snip'`).

Measured against rakudo 2026.09 (probes run by hand, see the PR): (a) a 6.e module's own
routines can call 6.e methods under a 6.d main; (b) a 6.d module's routines can call `.snip`
under a 6.e main; (c) once *any* 6.e compunit has loaded, even the 6.d main sees `.can('snip')`
and can call `.snip`. So method *visibility* is a property of the process (rakudo adds the 6.e
methods to the core types when the first 6.e compunit loads), while compile-time semantic
switches (multidim subscripts, `.hash` of ..., deepmap, ...) belong to the unit.

## 2. Decision

1. **Method visibility is process-wide.** `unit_revisions::revision_e_methods_visible()` is true
   when the running unit is 6.e or any unit of the process declared 6.e
   (`note_unit_language_revision`, a per-thread flag set when a loaded unit or the main program declares 6.e). The method-table
   rows (`RowFlags::in_force`, the owner memo), `can`/introspection and `snitch`'s raw-invocant
   rule use it.
2. **Everything else is per unit, by swapping the thread-local while a routine runs.** The loader
   records each unit's final revision against its source file (`note_unit_language_revision`: a
   module after its parse or cache replay, the main program after its parse). `RoutineStack`,
   the single funnel for frame push/pop/truncate, replaces the parser's current revision with
   the one recorded for a frame's `def_file` while the frame is live and restores it on
   pop/truncate. The swap is armed only when two recorded units disagree
   (`revisions_mixed`); a program whose units share one revision pays one relaxed atomic load
   per push.

## 3. Consequences

- Module code written for 6.e behaves as 6.e under a 6.d caller (and the reverse), for both
  methods and the semantic gates.
- Closures declared in a module run under their declaring file's revision as long as their
  frame carries a `def_file`; frames without one (inlined bare blocks) belong to the enclosing
  routine.
- `EVAL` keeps inheriting the caller's *current* revision, which is now the running routine's.

## 4. Alternatives rejected

- *Revision on `CompiledCode`, restored at each call site.* The call funnels are many (fast,
  light, positional-light, method, closure, thunk), and a missed one silently runs under the
  wrong revision; the frame stack is the one place every one of them already goes through.
- *Process-wide for everything.* Wrong for the semantic gates: a 6.d module under a 6.e main
  must keep 6.d behavior (rakudo compiles it against CORE.d).
