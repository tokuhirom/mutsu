# `<$var>` holding a Str no longer forces a fate in LTM ranking

`token TOP { <equation> | <expression> }` with `expression` reaching, through
a nested alternation, a branch shaped `rule chain { <before .+? <$ops>> ...
}` (`$ops` a `my`-declared string like `"['+'|'-']"`) ranked `expression`
above `equation` even when `equation`'s own declarative prefix was the
longer, properly bounded one. `Math::Symbolic`'s grammar uses exactly this
shape (`infix_chain_*` rules with `<before .+? <$in_ops_a>>`), and its
`t/01-basics.t` failed to parse `'x+y=1'` and similar expressions under
mutsu.

The root cause: ADR-0046's Decision 2 marks a `<$var>` regex-value
interpolation as an unconditional fate for LTM purposes (`$rx = rx/.../`,
its own probe S, dual-oracle verified against `raku`), and the
implementation applied that same marking to a `$var` holding a plain Str
too. But the two are not alike — a Regex-typed value's own AST is as opaque
to the NFA builder as a called routine's body, while a `$var` holding a Str
is re-parsed into a real `RegexPattern` right there at the `<$var>`
interpolation's own parse site, exactly as knowable as if the string's
content had been written literally in the same spot. Marking it a fate too
meant that, once the (correct, ADR-0125) NFA simulation walked an unbounded
quantifier ahead of it (`.+?`), the same fate node was revisited at every
position the quantifier explored, and each revisit stretched the branch's
measured prefix further — as far as the end of the subject, in the reported
grammar, comfortably outranking a bounded sibling branch.

The fix only marks the interpolation as a fate when the resolved value is
actually Regex-typed; a plain Str now measures normally, recursing into its
freshly-parsed AST exactly like a hand-written literal in the same spot
would. Confirmed against the reduction from `Math::Symbolic`'s grammar
(`docs/adr/0046-...md`'s probe table gains probes V and W); `raku` itself
could not be reached from this container to re-confirm the exact reduction,
but `Math::Symbolic`'s own test suite is independently known to pass under
real rakudo with this shape.
