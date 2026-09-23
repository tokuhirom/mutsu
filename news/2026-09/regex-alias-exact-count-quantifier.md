# `$<a>=x**2` honours its exact repetition count

A sigil alias on an atom followed by an unspaced `**N` never matched
(issue #9198): `"xx" ~~ /$<a>=x**2/` was `Nil`, and `"xx2"` matched *all three*
characters. `$<a>=[x]**2`, `$<a>=(x)**2` and `$<a>=<?before x>**2` failed the
same way.

The static source-tree parser (`src/regex_tree.rs`), which only models the
single-character quantifiers `*`, `+` and `?`, attached the first `*` of `**`
to the aliased atom and then took the second `*` as a quantifier on the whole
alias, leaving the count `2` behind as a literal. For an un-aliased atom the
next `parse_atom` already declined the stray `*` and the pattern fell back to
the full runtime parser; the alias path bypassed that check.

`parse_quantifier` now refuses to consume a quantifier character that is
immediately followed by another quantifier or modifier character (`*`, `+`,
`?`, `!`, `:`), so every two-character spelling -- `**N`, frugal `*?`/`+?`,
the `!`/`:` modifiers -- makes the tree decline and the pattern goes to the
runtime parser, which handles all of them. `$<a>=x+?` was mis-parsed the same
way (as `$<a>=x+` made optional) and is fixed by the same change.

Pinned by `t/regex/syntax/alias-exact-count-quantifier.t`.
