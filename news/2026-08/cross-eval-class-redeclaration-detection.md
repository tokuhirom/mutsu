# Cross-EVAL class redeclaration detection

`class Foo {}; EVAL q[class Foo {}]; say "no redeclaration error"` used to
print `no redeclaration error` in mutsu — the `EVAL`'d class declaration
silently shadowed the outer one instead of being rejected. Real `raku` dies
at compile time with `Redeclaration of symbol 'Foo'.`, since a plain
(non-lexical) `class Foo {}` anywhere in a package, and a later `EVAL` in
that same package, both install into the same `GLOBAL` stash.

`check_eval_class_redeclarations` (`src/runtime/system_eval_redecl.rs`)
already detected redeclaration *within* one `EVAL`'d string, and had a
cross-boundary check against the live class registry (`self.has_class(...)`)
— but it was gated on the declared name containing `::` (package-qualified),
skipping every bare top-level name. The guarding comment called this "a
known scoping limitation": widening the check naively to bare names risked
false positives from block-scoped declarations that "leaked" into the
global registry.

## What the leak actually was

Probing confirmed the leak is real, but narrower than the comment implied.
A `class Foo {}` declared inside a `sub`/block body is genuinely
package-scoped once that code runs — real `raku` also flags
`sub f() { class Local {} }; f(); EVAL q[class Local {}]` as a redeclaration,
and even two sibling subs each declaring `class Local {}` (neither called)
conflict at raku's compile time. So a bare-name widening is *correct* for
that shape, not a false positive.

The actual false positive is `my class` (lexical): mutsu's registry stores a
lexical declaration's first occurrence under the same bare key as a
non-lexical class (`registry().classes["Foo"]`), and mutsu has no scope-exit
cleanup for that map. So `{ my class Foo {} }; EVAL q[class Foo {}]` — where
the lexical scope has long since exited — was about to be wrongly rejected,
even though real `raku` allows it (a lexical class never installs into the
package stash it would conflict with).

The fix adds `Registry::lexical_classes`, a set mirroring which registry
entries came from a `my`-scoped declaration (set/unset alongside the
existing `hidden_classes` bookkeeping in `register_class_decl`, with
matching snapshot/restore support for a rolled-back redeclaration attempt).
The cross-boundary check now requires the name to lack a lexical origin
*in addition to* the `EVAL`'d declaration itself not being lexical, and the
`::`-only restriction is dropped since the real risk it was covering is now
handled precisely.

## A second, distinct isolation gap

Broadening the check surfaced a second false-positive source while running
the full `t/` suite and a roast sweep: `eval-lives-ok`/`eval-dies-ok` (but
not `throws-like`) run their code in a way that must NOT conflict with an
outer same-named class. Reading the actual vendored `Test.rakumod`
(`modules/Rakudo-Core/lib/Test.rakumod`) explains why: `throws-like` runs
its string via `EVAL $code, context => $caller-context` (the caller's own
lexical scope), while `eval-lives-ok`/`eval-dies-ok` share a helper,
`eval_exception`, that calls plain `EVAL($code)` with no context — which
defaults to the lexical scope where that `EVAL` is *textually written*
(Test.rakumod's own module scope), not the calling program's. A `class Foo`
declared inside such an `EVAL`'d string therefore installs under a different
package than the caller's `Foo`, so it never conflicts — verified against
real `raku` for both helpers.

mutsu provides `Test` natively (BATTERIES.md rung 3), so there is no real
Test.rakumod compunit boundary to inherit from automatically. A new
`Interpreter::suppress_cross_eval_class_redeclaration_check` flag stands in
for it: set only on the throwaway nested `Interpreter` that
`test_fn_eval_lives_ok`/`test_fn_eval_dies_ok`
(`src/runtime/test_functions/eval_exception.rs`) construct to run their code
string, it gates off only the cross-boundary `has_class` check (same-`EVAL`
duplicate-declaration detection is untouched). `throws-like`'s nested
interpreter does not set the flag, matching its explicit caller-context
behavior.

## Fallout in local tests

The wider check caught three `t/` files whose expectations had been quietly
pinned to the *old*, buggy mutsu behavior rather than real `raku`:
`t/constructor-positional.t` and `t/trusts-undeclared.t` each redeclared a
plain class inside a `throws-like`/EVAL string that already existed outside
it — genuinely a redeclaration in real `raku` too (verified) — so their
inline duplicate `class` decls were removed / their class names were made
unique per subtest. `t/native-ctor-plan-invalidation.t` declared the same
`class D {}` in two sibling `EVAL` strings (also a genuine cross-`EVAL`
conflict in real `raku`, verified); both declarations were changed to
`my class D`, which real `raku` and mutsu both handle correctly for this
scenario.

## Verification

New pinned test `t/eval-class-redeclaration-cross-boundary.t` covers the
ticket's exact repro, a block-scoped (sub-body) class correctly still
conflicting, a `my class` from an exited lexical scope correctly NOT
conflicting, `my class` shadowing inside `EVAL` still working, two sibling
`EVAL`s of the same non-lexical class correctly conflicting, and
`eval-lives-ok`/`eval-dies-ok` correctly NOT conflicting — every case
verified against real `raku` first. Full local `t/` suite (3193 files,
29749 tests) is clean; a roast sweep of `S02-names-vars`, `S12-*`,
`S06-signature`, `S04-declarations`, `S11-modules`, and `integration` shows
only pre-existing, unrelated failures (none mention `Redeclaration`) —
`roast/S12-class/augment-supersede.t` (a whitelisted file that regressed
mid-investigation due to the `eval-lives-ok`/`throws-like` isolation gap
above) is fully green again once that fix was in, and `roast/S02-names-vars/
names.t`'s "can *not* redefine a class in EVAL -- classes are package
scoped" subtest now passes where it previously failed.

## The ticket is now closed out

`todo/tickets/remaining-language-feature-gaps.md`'s other bullets were
already stale: items 1 and 3 were marked resolved back on 2026-08-14, the
strict-mode undeclared-variable-read bullet under item 2 was resolved
2026-08-17, and `X::Redeclaration::Outer` (item 2's other bullet) turns out
to already be fully implemented (`src/parser/outer_redecl.rs`, pinned by
`t/outer-redeclaration.t`) — the ticket text just never caught up. With this
session's cross-`EVAL` fix, every bullet in the file is resolved, so the
ticket file itself is removed.
