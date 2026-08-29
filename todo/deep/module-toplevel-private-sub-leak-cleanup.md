# A module's own non-exported top-level routine leaks into the loading scope

`require`-ing or `use`-ing a module whose own package-less top-level `sub`
declaration is NOT `is export`ed still leaves that sub permanently callable,
bare, from the loading scope after the load finishes -- which raku does not
do (a package-less top-level `sub name {...}` is lexically scoped to its own
compilation unit).

Minimal repro (no deps):

```
# HelperMod.rakumod: sub helper() { say "mod helper" }
$ mutsu -e 'require HelperMod; helper();'
mod helper                      # raku: compile-time "Undeclared routine: helper"
```

`raku -e '...'` for the same source dies at compile time with `Undeclared
routine: helper`; mutsu prints `mod helper` and exits 0.

## Root cause

mutsu always registers a package-less top-level routine under the literal
`GLOBAL::<name>` function-registry key (see
`Interpreter::hide_toplevel_global_routines`'s doc comment in
`src/runtime/builtins_system_require.rs` for the full picture) -- the same
key any OTHER package-less top-level declaration, including the loading
scope's own, would use. Nothing currently distinguishes "this routine was
exported and should reach the importer" from "this routine was merely
declared, non-exported, and should stay private to its own compunit" at the
level of what remains reachable after a load finishes.

## Why this is filed separately, not fixed inline

`fix/require-mod-main-redeclaration` (PR landing alongside this ticket) fixed
the sibling, acute bug: a same-named package-less top-level routine in the
requiring script AND the required module raised a false `X::Redeclaration`
(or silently overwrote the caller's own binding when it didn't). That fix
added `Interpreter::hide_toplevel_global_routines` /
`restore_toplevel_global_routines`, which temporarily hide the loading
scope's own package-less top-level routines while the loaded compunit's body
runs, then restore them -- this alone is sufficient to stop the collision and
protect the caller's own bindings, and does not need to know anything about
export status.

Fixing (or leaking) a module's own NON-exported top-level routine was
initially attempted as a bonus generalization of the old `MAIN`-only
`remove_leaked_main_routines` sweep (delete any newly-registered,
non-`is export`ed top-level routine after a successful load). That
generalization repeatedly collided with other ambient/ephemeral top-level
mechanisms already in the codebase, each discovered only by a full `make
test` regression after the previous fix:

- **`sub EXPORT` itself** (`Interpreter::apply_module_export`) always
  registers as `GLOBAL::EXPORT`, is never itself `is export`ed, and is
  read+deleted by `apply_module_export` strictly *after* the module's own
  `run_block` returns -- a naive sweep deleted it before that call, and
  `apply_module_export`'s own `EXPORT` invocation commonly reads the
  module's OTHER private top-level subs too (`sub EXPORT($lang) { ... &greet-fr
  ... }`, `t/sub-export.t` "EXPORT selects among existing module subs by
  argument") -- sweeping those before the call also broke it.
- **NativeCall's prelude helpers** (`nativesizeof`, `nativecast`, `cglobal`,
  ...) are deliberately spliced as package-less `GLOBAL::` routines into
  every compunit that uses NativeCall (`PRELUDE_SUB_TRAIT` in
  `src/runtime/mod.rs`) and are never `is export`ed themselves -- a naive
  sweep deleted the first module's copy once a LATER, unrelated module
  finished loading (`t/add-method-qualified-and-invocant.t`,
  `t/bare-array-type-match.t`).
- **Multi candidates** are additive across compunits by design (several
  modules legitimately contribute candidates to the same shared name, e.g. a
  custom `multi trait_mod:<is>(...) is export` alongside `Test.rakumod`'s
  own) and export bookkeeping (`exported_subs`) is a per-name *set*, not a
  per-candidate record, so a before/after diff of it cannot tell "this name
  was already exported by an earlier module" from "this module's own new
  candidate happens to share that name" (`roast/integration/advent2011-day14.t`'s
  `Advent::MetaBoundaryAspect` fixture).

Each fix widened the exemption list without any confidence the list was now
exhaustive. Rather than keep discovering ambient mechanisms case-by-case
under `make test`, the generalization was reverted: `remove_leaked_main_routines`
stays scoped to `MAIN` only (the one name whose leak has a distinct, narrow
safety concern -- an un-exported leaked `MAIN` candidate must never remain
reachable at the dispatchable auto-dispatch key), and this repro is left
unfixed. See `news/2026-08/require-toplevel-routine-scoped-to-compunit.md`
for the full writeup of what shipped and what did not.

## What a real fix needs

An exhaustive audit of every mechanism that installs a package-less
`GLOBAL::<name>` function-registry entry outside of an ordinary user
`sub`/`multi sub` declaration, so a leak-sweep can positively identify "this
is one of the known persistent/ambient kinds, never touch it" rather than
guessing from a `newly-registered && not is-export` heuristic that keeps
being wrong. Candidates to grep for: `PRELUDE_SUB_TRAIT`, `EXPORT`-shaped
runtime hooks (`EXPORTHOW`, `sub EXPORT`), anything the compiler/registration
path treats as "GLOBAL regardless of package" by convention rather than by
explicit `unit module`/`package` declaration. Once that list is closed, the
same `remove_leaked_toplevel_routines` shape this ticket's PR tried (diff of
`exported_subs` before/after a specific load, scoped to package-less single
routines, explicitly excluding known-ambient names) should work.

## Impact

Narrow in practice: the leaked routine is only reachable if the caller
happens to call it bare by the exact same name the module used internally,
and roast's own suite did not surface this as a failure (no whitelisted file
regressed from leaving it unfixed). It is nonetheless a genuine
lexical-scoping divergence from raku, and the same root mechanism as the
already-recorded (fixed, but only for the module-vs-module direction)
cross-module-private-sub-redeclaration finding.
