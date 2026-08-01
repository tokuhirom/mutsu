# 19 `t/` files asserted against mutsu's lenient native `is`, not against Raku

mutsu's native `Test` provider stringifies `is`'s arguments more eagerly than
Raku's does, so a family of assertions in `t/` passed under mutsu and nowhere
else — not under rakudo's real `Test.rakumod`, and not under `raku`. They are
**test-file** bugs, and each correction makes the test more faithful, so none of
them had to wait for the Test-vendoring flip
(`todo/tickets/vendor-real-test-module.md`).

Three shapes, 40 assertions, 19 files. Every corrected file was verified three
ways — mutsu's native provider, the aliased upstream `Test.rakumod`, and `raku`:

**A type object compared against its gist spelling** — 35 assertions in 15
files. `is 42.WHAT, '(Int)'` compares `$got.Str`, and a type object's `.Str` is
the empty string with a warning, not its `.gist`. All of them now read
`is 42.WHAT.gist, '(Int)'`, matching `t/type-objects.t`, which already had it
right.

`t/lock.t` was the one exception: it wanted the *qualified* name, and `.gist` of
a nested type object is its short name in raku too (`Lock::Async.WHAT.gist` is
`(Async)`, in both implementations). It asks `.^name` instead.

**`Empty` compared against `Nil`** — 4 assertions in 4 files. `andthen` and
`notandthen` yield `Empty`, an empty `Slip`, when they skip their RHS; so does a
routine whose body ends in a statement-modifier `if` that does not fire, which
is what `t/operator-adverbs.t`'s user-defined `infix:<->` does. `is $x, Nil`
passed natively and failed under `raku`; `is-deeply $x, Empty` holds under both.

## One real compiler bug fell out of it

Correcting `t/new-operators.t` to expect `Empty` made it fail under mutsu — for
real this time. The compiler's `andthen` arm loads an empty `Slip` when it skips
its RHS, but the `notandthen` arm right below it loaded `Nil`
(`src/compiler/expr_binary.rs`). So `10 notandthen 42` was a one-element list in
list context where raku's vanishes. The `notandthen` arm now loads the same
empty `Slip` its sibling does.

This is the pattern the whole lenient-`is` pass is worth doing for: the loose
assertion was hiding a genuine divergence, and tightening it surfaced the bug
immediately.

## Effect on the sweep

These 19 files were the bulk of the sweep's "`raku` fails it too" bucket (27
files). Eight remain in that bucket and are *not* this problem — they are listed
for individual triage in
`todo/tickets/local-tests-rely-on-a-lenient-native-is.md`.
