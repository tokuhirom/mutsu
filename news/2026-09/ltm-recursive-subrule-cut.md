# LTM measurement cuts a recursive subrule call, as Rakudo's NFA does

When mutsu ranks the branches of a `|` alternation, it measures each branch's
declarative prefix by walking the matcher in a mode that executes nothing. A
subrule call in the branch is walked into. That walk used to follow a recursive
rule all the way down. Rakudo does not: when it builds a rule's NFA it inlines
each called rule once. A call to a rule that is already being inlined becomes a
fate, which ends that path of the prefix where the call stands. ADR-0022 already
lists this "recursion cut", but it had not been implemented.

The difference is observable:

```raku
grammar H { token A { 'a' <A>? 'b' | 'q' }; token T { [ <A> | 'aab' ] } }
say H.subparse("aabb", :rule<T>);   # raku and now mutsu: 「aab」 (was 「aabb」)
```

Rakudo measures `<A>` as one character (`'a'`, then the inner `<A>` is a fate).
`'aab'` measures three, so it wins. mutsu followed the recursion, measured `<A>`
as four, and picked it.

The three atom matchers that every subrule call goes through now keep the rules
being walked on a thread-local stack (`src/runtime/regex/regex_ltm_recursion.rs`).
A call to a rule already on that stack records a fate and fails its path. Each
measurement starts with an empty stack, so the ranked branch's own rule is still
inlined once, matching `raku` on `token A { 'x' [ <A> | 'x' ] }` and on a proto
whose candidate calls the proto. The measurement memo from #9579 now includes
the stack in its key, since the same branch at the same position can measure
differently under different enclosing calls.

This also removes most of the cost reported in #9617. Before, ranking `<A>` in
`[ <A> | . ]*?` walked the rule's whole nesting below the ranked position, so a
parse was cubic in the subject length. Now each ranking walks the subject once,
which is quadratic, the same order as Rakudo. Release build, `token` form, input
`'{' ~ ('{ab{c}d}' x $n) ~ '}'`, second run of each:

| n | before | after | raku |
| ---: | ---: | ---: | ---: |
| 64 | 0.37 s | 0.04 s | — |
| 128 | 2.57 s | 0.15 s | — |
| 256 | — | 0.59 s | — |
| 512 | — | 2.39 s | 0.056 s |

Doubling n multiplies both mutsu's and Rakudo's time by about 4. mutsu's
constant factor is still about 40 times larger than Rakudo's.
