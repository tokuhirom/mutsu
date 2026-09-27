# LTM ranking no longer over-measures a bounded `** min..max`

A `token`/`rule` alternation branch built from a bounded quantifier over a
single, captureless atom — `'a' ** 2..4`, or an exact `'a' ** 3` — ranked
too high against a same-length or shorter literal sibling. `token TOP { <x>
| 'aaa' }; token x { 'a' ** 2..4 }` picked `<x>` over the literal on a
subject with plenty of `a`s, where `raku` picks the literal.

The bug sat upstream of both places mutsu measures a declarative prefix
(the NFA and the walker): a plain, no-separator bounded `**min..max` over a
captureless single atom is string-unrolled, before either engine sees it,
into a bare alternation of fully repeated literals (`'aaaa'|'aaa'|'aa'`).
LTM ranking then treated that exactly like a hand-written `|`, crediting it
with its longest branch's real length and a nonzero `litlen` — but a
quantifier must do neither. Rakudo's own NFA builder marks its `litlen`
tracking as ended the instant it enters a quantifier, before laying down a
single repeat's edges, and its state-set simulation stops extending the
measured prefix once it converges, at `min(min + 1, max)` repeats.

The fix defers that shape to the native `RegexQuant::Repeat` parser instead
of unrolling it to text, so the quantifier's own machinery handles the
measurement: `build_counted` (the ADR-0125 NFA) and `walk_quant_chain` (the
walker) now cap a bounded repeat's declarative prefix at `min(min + 1,
max)`, and the existing "quantifiers end litlen" rule applies to it like
any other quantifier. An unbounded `**min..*` is untouched — Rakudo
measures it the same way mutsu already did. Real matching is unaffected
either way: a real match still walks the whole `min..max` range.
