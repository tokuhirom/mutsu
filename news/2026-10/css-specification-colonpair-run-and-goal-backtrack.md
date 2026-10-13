# CSS::Specification: adjacent colonpairs in `:name[...]` and `~` goal backtracking

Two parser-level gaps kept the CSS::Specification suite at 35/37 in `t/00basic.t`; both are fixed
and the suite now passes under mutsu exactly as under rakudo (`t/00basic.t` 37/37, `t/defs.t` 47/47).

- A run of colonpairs without commas inside a pair's bracket value, as in
  `:required[ :seq[...] :rule<length>, ]`, kept only the first pair: the items were parsed with
  `expression`, which reads the second `:pair` as an adverb on the first. They now go through the
  same `colonpair_run_item` path as a plain `[:a :b]`, both for the expression form and for the
  hash-literal form.
- In a `rule`, the inner term of `A ~ GOAL TERM` is followed by the goal, so a `||` inside it must be
  able to give its first branch back when the goal fails
  (`rule { '(' ~ ')' [ 'x' || 'x y' ] }` on `(x y)`). The implicit-whitespace pass skipped the
  backtrackable mark for a term at the end of the body; it now marks the inner term of a `~`.

Tests: `t/lang/adverbs/colonpair-adjacent-in-bracket-value.t`,
`t/grammar/rule-goal-inner-alternation-backtrack.t`.
