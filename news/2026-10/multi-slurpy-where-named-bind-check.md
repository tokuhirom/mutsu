# A `where` on a slurpy ranks with the named bind check, not as a refinement

`multi method measure(:font-size($_)!)` declared before
`multi method measure(*%misc where .elems == 1)` lost to the slurpy candidate
for `.measure(:font-size(2))`, so CSS::Properties' calculator ran its generic
`measure` body and called `CALL-ME` on a plain `Str`/`Int` (#10519). mutsu
counted the slurpy's `where` as a positional refinement, which outranked the
explicit named parameter.

Rakudo only treats a `where` on a *positional* parameter as narrowing. A
`where` on a slurpy just marks the candidate as needing a bind check, the
same as declaring a named parameter. Two such candidates tie, and the one
declared first wins. The sub ranking (`candidate_rank_key`) and the method
ranking (`pick_method_winner`) now both work this way:

- a slurpy's `where` moved out of the refinement count and into the named
  bind-check step;
- a slurpy hash no longer counts as a wide positional (it took the flat
  1000-point distance) or as an optional positional;
- the required-named step was dropped, because rakudo ties `(:$x!)` with
  `(:$x)` and runs the one declared first;
- candidates that tie while needing a named bind check are never reported
  as `X::Multi::Ambiguous` (`multi f(:$a)` / `multi f(:$b)` for `f()` runs
  the first). Purely positional ties are still ambiguous.

Pinned by `t/routines/dispatch/multi-slurpy-where-named-bind-check.t`.
