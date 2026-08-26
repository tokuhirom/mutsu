# A `.`-based character class now goes through the ordinary class-arithmetic accumulator

`<.-[a]-[b]>` and `<.-:letter-:digit>` — the any-character base `.` followed by
a *chain* of set subtractions — gave two different wrong answers (everything,
or nothing), while a single subtraction after the dot was correct.

## Root cause

The single-subtraction case was only right **by accident**. `<.-:letter>` had no
parse path either; it became `RegexAtom::Named(".-:letter")`, and the match-time
named-subrule resolver strips a leading `.` for the `<.rule>` non-capturing form
and then, having failed to find a rule called `-:letter`, falls through to a
"Unicode property fallback" that reads a `-:`-prefixed name as a negated
property. So `<.-:letter>` accidentally landed on "not a letter", which happens
to be the right answer for that one shape. A chain (`-:letter-:digit`,
`-[a]-[b]`) has no such fallback and produced garbage.

`parse_combined_class` — the real `+`/`-` accumulator that `<+alpha -[aeiou]>`
uses — was never reached, because nothing in the `<...>` dispatch recognised a
leading `.` as a class term.

## The fix

`<.` followed by `-` or `+` now routes into `parse_combined_class`, which seeds
the positive half with a new `ClassItem::Any`.

The `Any` *item* (rather than "leave the positive half empty", which the matcher
already reads as the universe) is what makes the chain come out right, because
**Raku's class arithmetic is not set arithmetic**. The positive and negative
halves accumulate independently and a character matches when it is in the
positive half and not in the negative half. So:

```raku
say "ab1 c".comb(/<.-[a]+[1]>/);   # (b 1   c)  -- 'a' stays excluded
```

True left-to-right set arithmetic would compute `(U \ {a}) ∪ {1}` = `U` and
re-admit `a`. Rakudo does not; `+[1]` only adds to the positive half, which
already contains everything, and the `-[a]` in the negative half still wins.
Modelling `.` as a positive item reproduces that exactly, where collapsing the
positive half to "empty means universe" would have been destroyed by the
following `+[1]`.

Verified against `raku` v2026.06 for the dot base with one, two and three
subtractions, a mixed property/bracket chain, a trailing union, and the
unaffected plain forms (`<-[a]-[b]>`, `<[a..z]-[a]-[b]>`, `<:letter-[a]>`).

A side effect worth noting: these classes are now `CompositeClass` atoms rather
than opaque `Named` ones, so `ltm_atom_mode` correctly sees a subtraction and
stops the declarative LTM prefix at them, as it already did for every other
written subtraction.

Pinned by `t/regex-engine-gaps.t`.
