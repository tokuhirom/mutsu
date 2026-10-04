# `<name=ident>` aliases resolve the builtin rule on any topic

An aliased `<succ=.ident>` / `<x=ident>` fell through to the "no such method" fallback
instead of the builtin `ident` rule (`<alpha> <alnum>*`) when the match target was a `Match`.
The aliased builtin path now shares `builtin_rule_end` with the rest of the regex engine.
Found via Font::AFM: `t/metrics-path.t` now passes (all four baseline files at parity).
