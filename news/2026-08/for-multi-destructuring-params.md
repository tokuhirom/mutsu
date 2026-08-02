# A pointy block can destructure more than one parameter

`for %h.kv -> [$target, $variant], [$expected, $desc] { ... }` did not parse.
mutsu reported `X::Syntax::Missing: Missing block`, which is what a `for` loop
says when no `{` follows its parameter list — and none did, because
`parse_for_params` handled a `[...]` / `(...)` destructuring pattern only as the
*whole* parameter list. That branch returned as soon as it closed the bracket,
leaving `, [$expected, $desc] {` unconsumed.

The same hole existed on the other side: a pattern in a *later* position
(`-> $a, [$b, $c]`) reached the general multi-parameter loop, which only knew how
to parse an ordinary parameter.

Both are now one shape. `parse_destructuring_or_plain_param` parses either kind
of entry, giving a pattern a synthetic `__for_unpack_N` name, and the
multi-parameter loop uses it for every position. The compiler's destructure
emission — previously inline in the single-pattern path — became the
`destructure_binds` closure, applied to each parameter that carries a
sub-signature *after* the per-element binds have run, so the pattern unpacks a
value that is already there.

The semantics follow from the existing multi-parameter rule: each entry takes one
element of the iteration chunk, and a pattern entry then unpacks the element it
took. So `for (1,2), (3,4) -> [$a,$b], [$c,$d]` runs once with `1234`, while the
single-pattern `for ((1,2), (3,4)) -> [$a,$b]` still runs twice.

This was the parse blocker on Cro::HTTP's `t/http-router.rakutest`, which walks
an `Array`-keyed hash exactly that way.

Pinned by `t/for-multi-destructuring-params.t` (checked against raku).
