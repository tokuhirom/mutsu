# A `<@var>` regex assertion no longer collapses a mutated lexical into one literal

A `<@var>` regex assertion (`/ ^ <@alts> $ /`) reads the array's VALUE at the moment the regex
literal is evaluated, splitting its elements into an alternation. `array_var_alternation_atom`
(`src/runtime/regex_parse_core.rs`) fetched that value with a bare `self.env.get(env_key)`, with no
attempt to dereference a `ContainerRef` cell.

The compiler boxes a lexical into such a shared cell when it is reassigned later in the same scope
(`@alts = <emu>;` after `my @alts = <cat dog>;`), so `@alts` in the earlier repro was a
`ContainerRef`, not a plain `Array`, by the time its regex literal ran. None of
`array_var_alternation_atom`'s match arms recognized `ValueView::ContainerRef`, so it fell through
to the catch-all arm — `crate::value::ArrayData::new(vec![value.clone()])` — treating the *whole
cell* as a single alternation element. Stringifying it produced the array's rendered form as ONE
literal pattern (`"cat dog"`) instead of two alternatives (`cat|dog`), so a match against a single
word like `"cat"` failed even though the array actually held it.

A never-reassigned lexical (`my @only = <cat dog>;`, nothing else touching `@only`) is never boxed,
so it already worked — which was the one case narrowing this down. A single-element array after
reassignment (`<emu>`) also happened to stringify identically whether treated as a whole cell or
correctly split into (one) alternative, which is why only the pre-reassignment match in the
original repro showed the bug.

The fix mirrors what `<$var>`'s sibling code path a few lines below already does for the same
reason (`.into_deref()`, following up on the `stored-regex-loses-its-defining-scope-lexicals`
fix): `.deref_container()` the fetched value before matching on its `ValueView`. Two things were
worth re-measuring against real rakudo rather than assuming from the issue text alone: a `<@var>`
assertion, like a bare `$var` interpolation, re-reads the array live on every match (a reassignment
made after a stored regex's construction but before a later match against it IS visible) — a
guess to the contrary while writing the regression test was quickly proven wrong by the oracle.

Pinned in `t/regex/regex-subpattern-parse-memo.t` (a match before and after a same-scope
reassignment) and `t/regex/regex-stored-closure-scope.t` (a stored `rx/.../` reassigned before a
later match against it).

Fixes #8040.
