# A quantified sigil alias on a subrule call captures per iteration

`grammar H { token foo { x }; token q1 { $<a>=<foo>+ } }` parsed `'xx'` with
`$<a>` as one Match spanning both characters; rakudo gives a List of two
Matches, one per iteration (issue #9128). The same held for builtin subrules
(`$<a>=<alpha>+`), dot-suppressed ones (`$<a>=<.alpha>**2`), the `<?foo>`
spelling (`$<a>=<?foo>+`, which is `$<a>=<foo>+` since #9112), and
`@<a>=<.alpha>+`.

In rakudo a sigil alias on a subrule atom is `subrule_alias`: the call itself
is renamed `a=foo`, so the quantifier after it repeats a capturing call, the
same as `<foo>+` does for `$<foo>`. Only a non-subrule atom (`$<a>=\w+`,
`$<a>=[\w]+`, a char class or a Unicode property) captures the whole
quantified span as one Match.

The legacy regex parser's `wrap_named_quant` wrapped every non-group
quantified atom under a user sigil alias into a single capturing span. The
`'<'` arm now records when the aliased atom is a subrule call (an identifier,
optionally after `.`, once the `?` of an aliased `<?foo>` has been dropped),
and that case is left to the quantifier loop, which captures per iteration.
The same flag makes `@<a>=<.alpha>` a List, as `@<a>=<alpha>` already was.

Pinned by `t/regex/regex-sigil-alias-quantified-subrule.t`.
