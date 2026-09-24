# Regex: `\h` includes U+180E, and an excluded spacing mark no longer joins its base

Two regex divergences from rakudo left over from the ADR-0118 §2.5 character
class probe (#9229):

1. **`\h` missed U+180E MONGOLIAN VOWEL SEPARATOR.** MoarVM still lists it as
   horizontal whitespace. The set was spelled out twice, in the `HorizSpace` and
   `NegHorizSpace` arms of `regex_eval_class.rs`; it now lives once, as
   `cclass::is_horiz_space`, and `\h`, `\H`, `<[\h]>` and `<-[\h]>` all read it.

2. **`"x\x[102B]" ~~ /x>>/` failed.** The issue guessed a word-boundary bug, but
   the boundary logic was fine: the literal `x` itself did not match. The regex
   engine's `grapheme_end` treated every General_Category `M*` codepoint as
   extending the cluster, while UAX #29 excludes about thirty `Mc` codepoints
   (U+102B, U+102C, U+1038, U+1062..U+1064, ...) from `SpacingMark`. `.chars`
   already followed UAX #29 and answered 2, so the regex saw one grapheme where
   the string had two. `grapheme_end` now hands a cluster containing an `Mc`
   to the real UAX #29 segmentation, as it already did for Indic viramas.
   All 260 BMP `Mc` codepoints now agree with rakudo on `.chars`, `/x>>/` and
   `/^.$/`.

Pinned by `t/regex/regex-horiz-space-and-spacing-mark.t`. Closes #9233.
