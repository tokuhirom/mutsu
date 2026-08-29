# A required module's top-level MAIN no longer collides with the caller's

`require`-ing (or `use`-ing) a module whose own top-level `sub MAIN` collided
with the requiring script's own top-level `sub MAIN` raised a spurious
`X::Redeclaration`:

```
$ mutsu -e 'sub MAIN($a, $b, *@c) { say "main called" }; require HasMain;' a b c
Redeclaration of routine 'MAIN'. Did you mean to declare a multi-sub?
```

`raku` raises nothing: the loaded module's own `sub MAIN` never runs (a
non-exported `sub MAIN` in a `require`d/`use`d module is never the program's
MAIN), and the caller's own MAIN still dispatches normally afterward. This is
`roast/S06-other/main.t` test 2, `lives-ok { require HasMain }, 'MAIN in a
module did not get executed'`, which fails only under `MUTSU_REAL_TEST=1`
(the vendored upstream `Test.rakumod` actually asserts the message the native
`lives-ok` provider was papering over).

## Root cause: not MAIN-specific

Reducing the case with no `Test` involved at all showed the bug has nothing
to do with `MAIN` in particular — an ordinary top-level `sub helper` collides
identically:

```
# HelperMod.rakumod: sub helper() { say "mod helper" }
$ mutsu -e 'sub helper() { say "caller helper" }; require HelperMod; helper();'
Redeclaration of routine 'helper'. Did you mean to declare a multi-sub?
```

Raku scopes a package-less top-level `sub name {...}` **lexically to its own
compilation unit** — it is not installed as a shared package-stash entry the
way an `our sub`/package-scoped routine is. Two independent compunits (a
script and a module it requires, or two sibling modules) that happen to
declare a same-named top-level routine must not collide with each other, and
neither's own declarations should overwrite the other's.

mutsu, however, always registers a package-less top-level routine under the
literal `GLOBAL::<name>` function-registry key and, for a block-scoped load,
under the bare `&name` env key too — the *same* keys the requiring/using
script's own top-level declarations use. Loading a module whose body
registered a same-named routine under that shared key either collided with
(raised `X::Redeclaration` against) or silently overwrote the caller's own
binding.

This is the same root mechanism as the previously-recorded
"cross-module-private-sub-redeclaration" finding (two sibling modules sharing
a private helper name), just triggered from the caller-vs-module direction
instead of module-vs-module.

## Fix

`Interpreter::hide_toplevel_global_routines` temporarily removes every
already-registered package-less top-level **single** (non-`multi`) routine
(and its bare `&name` env binding) before a `require`d/`use`d compunit's own
body runs, so that compunit's own registrations land on a clean slate — no
collision, and no risk of clobbering what was hidden.
`Interpreter::restore_toplevel_global_routines` puts the hidden entries back
once the body finishes (success or failure), so the caller's own bindings are
exactly what they were before the load.

**Multi candidates are deliberately left alone entirely** by this hide/restore
— exported or not. A `multi` is additive across compilation units by design:
several independent modules legitimately contribute candidates to the same
shared name (a custom `multi trait_mod:<is>(...) is export` alongside
`Test.rakumod`'s own `trait_mod:<is>` candidate is exactly this shape, and
`roast/integration/advent2011-day14.t`'s `Advent::MetaBoundaryAspect` fixture
does exactly that). Hiding a multi's existing candidates before a module's own
body registers a new one, then blindly restoring the old ones by key
afterward, silently overwrote the new candidate whenever it landed on the
same derived slot key as an old one — which is exactly what an earlier
version of this fix did, breaking the custom trait `MetaBoundaryAspect`
registers (its `entry`/`exit` method-boundary hooks stopped firing). `EXPORT`
is excluded from hide/restore for the same "ambient shared mechanism, don't
touch it" reason — see below.

**The generalized "reap a module's own non-exported top-level routines after
it loads" cleanup this fix originally attempted was reverted.** It repeatedly
collided with other ambient/ephemeral top-level mechanisms already in the
codebase — a module's own `sub EXPORT`, and the helper subs its body commonly
reads before installing exports (`t/sub-export.t`); NativeCall's prelude
helpers (`nativesizeof`/`nativecast`/...), spliced as package-less `GLOBAL::`
routines into every compunit that uses NativeCall and never themselves `is
export`ed (`t/add-method-qualified-and-invocant.t`,
`t/bare-array-type-match.t`) — each only discovered by a fresh `make test`
regression after the previous exemption was added. Rather than keep
discovering ambient mechanisms case-by-case, that generalization is deferred
to `todo/deep/module-toplevel-private-sub-leak-cleanup.md`, and the leak
cleanup this PR ships stays scoped to `MAIN` only — the pre-existing,
long-safe `remove_leaked_main_routines` behavior, unchanged: a leaked,
non-exported `MAIN` candidate must never remain reachable at the dispatchable
auto-dispatch key, which is a distinct safety concern from ordinary
leak-prevention. **A module's own non-exported top-level `sub` (other than
`MAIN`) can still leak into the caller's scope after a successful load** —
that is the known, still-open gap the follow-up ticket tracks; it predates
this fix and is not made worse by it.

## Verification

- `roast/S06-other/main.t`: all 23 assertions pass under both the native TAP
  provider and `MUTSU_REAL_TEST=1`.
- `roast/integration/advent2011-day14.t`: all 8 assertions pass under both
  providers (was regressed by an earlier version of this fix, see above).
- New pins (both green under real `raku`):
  `t/require-toplevel-routine-scoped-to-compunit.t` (4 assertions), covering
  both `require` and `use`, and both `sub MAIN` and an ordinary `sub helper`
  (fixtures: `t/lib/ToplevelMainCollision.rakumod`,
  `t/lib/ToplevelHelperCollision.rakumod`);
  `t/require-toplevel-multi-candidate-not-leaked.t` (2 assertions), covering
  the multi-candidate-across-modules case (fixtures:
  `t/lib/SharedMultiHost.rakumod`, `t/lib/SharedMultiContrib.rakumod`).
- `make test` (3551 files) and a targeted native-provider roast sweep passed;
  see the 2026-08-29 entry in `todo/deep/vendor-real-test-module.md` for the
  full before/after sweep numbers under both providers.
