# The compiled regex engine takes quantified captures and the position-only matcher

The second part of ADR-0135 Slice A ([#10251](https://github.com/tokuhirom/mutsu/issues/10251))
widens what the compiled regex engine covers, in three directions.

**Quantified captures.** `(\w)+`, `[ (\w) (\d) ]+` and the like now compile. They are captures
one level deep under `*`, `+`, `**` or frugal and ratcheted forms. The program does what the
walk's `walk_quant_chain` does, in the same order:

- mark every name under the quantifier as list-valued before the first iteration;
- push one slot per capture group per iteration;
- fold the slots into lists at the loop's exit with the walk's own `fold_quantified`.

Backtracking out of the loop rewinds the fold with the rest of the capture trail.

**Captures under `?`.** `(a)?`, `$<x>=[a]?` and `[ (a) (c) ]?` compile too. The matched arm
applies the alias over what it matched. The empty arm reserves the atom's slots (Nil, or an empty
list under a nested list quantifier) exactly as `walk_zero_or_one_zero_arm` does.

**The position-only matcher.** `regex_match_nocap.rs` serves `.comb` without captures,
`find_first` and the walk's own group probes. It now asks the compiled engine first. That fixed a
bug on the way: this matcher never honored `:r`. So `"aaax bbx".comb(/ :r \w+ 'x' /)` found two
matches, `aaax` and `bbx`, where rakudo finds none. The compiled engine cuts the ratcheted `\w+`
and agrees with rakudo. It is pinned by `t/regex/match/regex-comb-ratchet-no-capture.t`.

`MUTSU_RX_DIFF=1` agreed with the walk on every file of `t/regex/`, `t/grammar/` and the
whitelisted `roast/S05-*`.
