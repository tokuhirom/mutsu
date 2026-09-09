# A package-qualified name no longer leaks through a transitively-`use`d module

```raku
# InnerConst.rakumod: unit module InnerConst; constant INNER-CONST = 42;
# OuterConst.rakumod: use InnerConst; unit module OuterConst;

use OuterConst;   # never `use InnerConst` itself
say InnerConst::INNER-CONST;   # raku: Could not find symbol   mutsu was: 42
```

The bare-name half of this divergence (#7555 item 1(b)) closed across three
earlier slices — #7743 (imports/classes/roles), #7764 (package names), #7791
(constants/enum values) — so a *bare* `InnerConst` reference already correctly
went undeclared for an importer that only `use`d `OuterConst`. The
**package-qualified** form (`InnerConst::INNER-CONST`, `InnerConst::InnerClass`,
`InnerConst::InnerEnum`, a qualified call to one of `InnerConst`'s subs, ...)
still resolved, because mutsu's package symbols — `our`-scoped constants/vars,
classes, roles, enums — live in process-global stores keyed by their qualified
name, with no notion of who may see an entry. Rakudo installs a `use`d package
into the *importing compunit's* `MY::` only, so a compunit that never `use`d
`InnerConst` itself has no path to `InnerConst::anything`, even through a
module it did `use` that in turn `use`d `InnerConst`.

## The fix

Two new per-compunit tables, mirroring the existing `prelude_declaring_units`/
`prelude_visible_here` mechanism built for the analogous NativeCall-prelude
visibility gate (#7612):

- `package_declaring_units`: which compunit a `use`/`need`/`require`d
  top-level package belongs to (first `::`-segment granularity, matching the
  coarseness the three earlier bare-name slices already use).
- `compunit_visible_packages`: which such packages a given compunit earned
  visibility to, by `use`ing them *directly* — populated at the end of a
  successful module load, whether that's the first load (`load_module_inner`)
  or a re-`use` of an already-loaded module (which used to skip the grant
  entirely, since nothing new needs registering there for the OLDER
  bare-name/class-alias mechanisms).

`Interpreter::qualified_name_visible_here` consults both, walking the `EVAL`
parent chain exactly as `prelude_visible_here` does, and gates the two
chokepoints where a source-written qualified name resolves: the bareword term
path (`exec_get_bare_word_op` — constants, enum values/types, classes, roles)
and the qualified function-call path (`exec_call_func_op_inner`).

## The harder half: which compunit is "running" during a module load

Getting the *visibility* gate right exposed a second, sharper bug in how
mutsu decides "which compunit is executing right now" while a module's own
top-level mainline is running. `Interpreter::executing_unit_sym` prioritizes
the topmost `routine_stack` frame — correct for an ordinary call, but wrong
for a module body, which runs via `run_block` and pushes no frame of its own.
`DBIish.install-driver('SQLite')` does a dynamic `require ::($module)` from
inside a method; the loaded module's own top-level `use NativeLibs;` (and a
`constant LIB = NativeLibs::is-win ?? ... !! ...;` reading a qualified name
from *another* nested module) then misattributed to `install-driver`'s own
compunit instead of to the module whose mainline was actually running,
because `install-driver`'s stale frame was still on top of `routine_stack`.

`module_loading_unit_stack` now records `(compunit, routine_stack depth)`
around every module load's `run_block`; `executing_unit_sym_for_module_load`
trusts the stack's top only while that depth hasn't grown since (nothing has
been *called* since the module's mainline started), and falls back to
`executing_unit_sym` the moment a routine call — possibly into a different
compunit entirely — happens in between.

## Verification

Extends `t/module-transitive-use-does-not-leak-types.t` (the pin for the
bare-name half) with the qualified-name assertions, rather than adding a
parallel file, per the issue's own instruction. `make test` (41,855
assertions) and `make roast` are unaffected beyond the fix; three module-load
edge cases in `t/` that depend on the mainline-attribution fix above
(`t/attr-default-file-scoped-call.t`, `t/enum-member-nested-module-load.t`,
`t/prelude-helper-not-block-lexical.t`) needed no changes themselves — they
started passing once `executing_unit_sym_for_module_load` existed.

Filed #7803 for an unrelated pre-existing gap found while extending the pinned
test: a `unit module`'s plain `sub NAME() is export` (as opposed to `our sub`)
is not callable via `Module::NAME()` qualified-call syntax at all, regardless
of visibility.
