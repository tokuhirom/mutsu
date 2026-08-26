# `$¢` was never missing — `$$` followed by `{` was misparsed as `${…}`

The ticket read `my $c; 'abc' ~~ /.$${ $c = $¢ }/; say $c` giving `(Any)` as
"`$¢` is unimplemented". It is implemented: the same regex with one space,
`/. $$ { $c = $¢ }/`, already left `$c` holding `｢c｣`. The variable was fine;
the *pattern* never parsed.

## Root cause

`interpolate_regex_scalars` (`src/runtime/regex_parse_modifier.rs`) walks the
pattern text substituting interpolated scalars, and its `$` branch accepts
`${name}` as a braced variable reference. In `.$${ $c = $¢ }` it consumed the
first `$` as a literal, then read the second `$` plus the *code block* as
`${ $c = $¢ }` — a variable whose name is the string `" $c = $¢ "`. That name
resolves to nothing, so the anchor and the whole block were replaced by an empty
interpolation: the match failed and the block never ran. `$$` is the
end-of-line anchor (`Language/regexes.rakudoc` line 1180), and the `{` after it
opens a code block.

## Fix

Both pattern-text scalar scanners — `interpolate_regex_scalars` and its
bound-parameter sibling `interpolate_bound_regex_scalars`
(`src/runtime/regex/regex_interpolate.rs`) — now recognise `$$` and copy it
through verbatim before the `${…}` branch can claim the second `$`. The
structural tokenizer already handled `$$` correctly; it simply never saw it.

While confirming the semantics, `$¢` was checked against raku across cases: it
is a `Match` over the same span as `$/` (`from` = the current match start, `pos`
= the cursor), carries the captures matched so far, and is a *distinct object*
from `$/` (`$¢ =:= $/` is `False`). mutsu matches on all of those.

Pin: `t/regex-embedded-code-blocks.t`.
