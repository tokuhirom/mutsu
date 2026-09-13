# A regex character class works on graphemes, not codepoints

Raku strings are NFG, so a character class and a Unicode property both test a whole grapheme.
[#8342](https://github.com/tokuhirom/mutsu/issues/8342) reported two consequences mutsu had neither
of, and they are one fix — the first alone breaks what the second makes representable.

```
                                            rakudo   mutsu (before)
so "a\x[094D]b" ~~ /<:Mn>/                   False    True
so "a\x[094D]b" ~~ /<[\x[094D]]>/            False    True
so "\x[1ea2]\x[5b4]" ~~ /<[\x[5b4]]>/        False    True
so "क्ष" ~~ /<[क्ष]>/                          True     Runtime error
```

**No atom may start inside a cluster.** The combining mark in `"a\x[094D]b"` is part of the first
grapheme, not a position anything can begin at. `#8341` had enforced that for literal atoms only;
the guard now covers the class and property arms too.

**A class may hold a multi-codepoint grapheme.** `<[क्ष]>` is one entry. mutsu rejected it outright
with *"Cannot use क् as a range endpoint, as it is not a single codepoint"* — conflating a legal
class **entry** with an illegal range **endpoint**. A cluster is still refused as a range endpoint,
which the pin checks.

`ClassItem` gained a `Grapheme(Box<str>)` variant for the clusters NFC does not collapse to a single
`char`, compared against the subject at the atom (the only place with the text to compare), both
sides in NFC.

## Segmentation lives in one place

The interesting part was *not* adding the variant. `compose_char_class_items` already grouped a base
character with the combining marks after it, and that rule is wrong for `क` + `्` + `ष`: UAX #29 GB9c
joins the consonant that follows an Indic virama, so those three codepoints are **one** grapheme, not
a cluster plus a stray consonant. A first attempt that segmented at parse time produced the entry
`क्` and left `ष` behind, and the class then matched nothing.

So the class parser no longer segments at all — base and marks go in as ordinary `Char` items, and
`compose_char_class_items` groups each run of them with `grapheme_end`, the same function the matcher
uses. One rule, one place. (`grapheme_end` widened from `pub(super)` to `pub(crate)` for it.)

With one restriction that the suite had to teach: a cluster starts only where the *next* codepoint is
a combining mark. Grouping every run `grapheme_end` would join is wrong inside a class, because
adjacent entries there are separate **alternatives**, not one grapheme. `<[ \x[D]\x[A] \x[A] ]>` holds
`\r\n` and `\n` as two entries and `"\r"` matches the first — while `grapheme_end`, correctly for
subject text, treats `\r\n` as one cluster. `roast/S05-modifier/ignoremark.t` test 60 is precisely
that case, and it went red until the guard was added. Requiring a combining mark to open a cluster
keeps the two apart while still letting the full segmentation pull in the consonant after an Indic
virama, since the virama *is* a combining mark.

## What the ticket predicted, and what it got wrong

Its risk assessment was exact: applying the boundary guard turned `roast/S05-mass/named-chars.t`
test 155 red, for precisely the reason given — `\c[LATIN CAPITAL LETTER A WITH HOOK ABOVE,HEBREW
POINT HIRIQ]` names one grapheme, and while it was stored as two codepoints it could only match by
starting inside the cluster. The `Grapheme` variant is what makes it representable. All six
whitelisted files it names are green.

Two smaller notes:

- It warned that `regex_prefilter.rs` and `regex_ltm_rank.rs` have `_ =>` arms that would silently
  mishandle a new variant. Neither file references `ClassItem` at all. The catch-alls that do exist
  (`strip_marks_class_item`, the casefold expansion) pass a `Grapheme` through unchanged. The
  audit's conclusion that this was "conservative" was too comfortable, though — the `ignoremark`
  regression above surfaced in that very area, and it was the full `make roast` that caught it, not
  the reading.
- It listed `so "a\x[301]" ~~ /<[a]>/` as needing the class to consume a whole grapheme. It does not:
  the existing `class_has_only_exact_chars` guard already refuses to match the base of a synthetic
  grapheme, and that case is `False` both before and after. It is pinned anyway.

## Pin

`t/regex/syntax/regex-char-class-grapheme.t` — 12 tests, green under mutsu and under real Rakudo:
the three boundary cases, a mark after a control (which UAX #29 GB4 makes its own grapheme, so it
*does* match), the base-of-a-cluster case, the multi-codepoint entry in both the literal and
`\c[A,B]` spellings, and the range endpoint still being refused.
