# The capture-alias map stops cloning two strings per candidate

A non-suppressing alias — `<val=word>`, `<tags=tag-directive>` — files its match
under both names, and the engine records the alias-to-original mapping in the
capture accumulator so the Match builder can reproduce it. That map was
`HashMap<String, String>`.

Nothing about it wanted owned strings. Both halves of an entry are capture
names the memoized lookup spec already holds as interned `Symbol`s
(`spec.capture_sym`, `spec.lookup_sym`), and the only consumers are the two
places that turn the finished map into a Match attribute, which resolve the
names back to text once. In between sat the regex engine, which clones a whole
`RegexCaptures` per accepted candidate and merges a delta per matched atom —
so every alias cost two `String` allocations on each of those, plus a third for
the undo record the trail keeps so a backtrack can restore it.

Keying it by `Symbol` removes all of them. On a 60-row YAML document under
callgrind, `String::clone` calls go **235,346 -> 144,469 (-39%)**, and
instructions retired **1,317,486,807 -> 1,275,306,641 (-3.20%)** — the largest
single item of the round, for a type change with an eleven-error blast radius
that the compiler enumerated.

The undo record (`Undo::AliasRestore`) carries symbols now too, which also
shrinks it: a backtrack over an aliased capture used to allocate and free two
strings just to put the previous entry back.

Pinned by `t/grammar/grammar-capture-alias-across-backtracking.t` (verified
against rakudo 2026.07): an alias inside an alternation under a separated
quantifier, so the engine builds, merges and rewinds the alias record several
times per item before the parse commits — both names resolve, the rejected
branch leaves nothing behind, and a quantified alias still collects the whole
list under each name.

Refs [#7576](https://github.com/tokuhirom/mutsu/issues/7576).
