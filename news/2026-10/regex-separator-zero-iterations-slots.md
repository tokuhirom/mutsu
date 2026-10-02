# A zero-iteration separated quantifier keeps its capture slots

`"" ~~ / (\d)* % ',' /` left `$/.list` as `()`, where rakudo (and mutsu's
own unseparated `(\d)*`) gives `([],)`. When a separated quantifier matched
nothing, every engine built its capture delta from the quantified *names*
alone, with strides of 0, so the atom's and separator's capture groups lost
their positional slots. Any later group then shifted into the wrong slot.

All four zero-iteration paths now fold an empty chain with the real atom and
separator strides, which reserves each slot as an empty list:

- the walk's eager candidate list (`match_separated_quantifier`);
- its lazy chain walk (`SepChainWalk::names_delta`);
- the ratchet scan;
- the compiled engine's `rx_sep_fold`.

This covers `*% `, `%%`, frugal, ratcheted and ranged quantifiers (#10534).

Pinned by `t/regex/regex-separator-zero-iterations-slots.t`, which runs under
both engines.
