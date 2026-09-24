# A `?` that skips a subrule call publishes no capture

`"xx" ~~ /<after x>? x/` matches at position 0, where `<after x>` cannot
succeed, so the `?` takes zero iterations. raku publishes no capture at all;
mutsu published an empty `after` Match (and an empty `a` under
`$<a>=<after x>?`), as if the zero-width call had run (#9212). The same was
true of a builtin subrule: `"1" ~~ /<alpha>? 1/` showed `alpha => ｢｣`.

The `?` matcher already knew the rule: a name on a bare subrule call stays
absent on the zero branch, while a `$<x>=` alias on a plain atom (`[y]`,
`<[cd]>`) still renders an empty Match. But it told the two apart by atom kind
alone. A builtin subrule parses as a `CharClass` atom and a capturing
lookaround as a `Lookaround` atom, exactly like a char-class alias, so both
fell on the "always renders" side. `RegexToken` now carries
`subrule_call_capture`, set by the legacy parser when the name comes from a
builtin call (bare, or as the secondary name under an alias) and by the regex
tree lowering for a capturing lookaround. The zero branch consults it. The tree
lowering also no longer wraps `$<a>=<after x>?` in an always-running group:
like an aliased capturing group, an aliased call stays per-iteration.

Pinned by `t/regex/match/regex-optional-subrule-zero-capture.t`.
