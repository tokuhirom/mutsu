# A user infix spelled with hyper markers (`infix:«>>>»`) wins over a hyper reading

`OneSeq` exports two list-associative operators, `infix:«>>>»` and
`infix:«<<<»`, and its test writes `my @e = @a >>> @a >>> @a;`. mutsu failed
with this #7988 cluster's generic `Confused. expected statement: ... expression
after hyper operator ...` message.

A single `@a >>> @b` already parsed, but in a chain the concatenation-level
loop asked `parse_hyper_op` first. It read `>>` as a hyper opener and then
found a "closing" `>>` inside the NEXT `>>>`, producing a bogus base operator
`> @a ` that spans whitespace, and the hyper right-hand-side parse then failed.

`parse_hyper_op` now defers to a user-declared infix that matches at the same
position when that infix is at least as long as the hyper reading, or when the
hyper reading spans whitespace (which no hyper operator can) — longest-token
matching, as in rakudo. Real hyper operators (`>>+<<`, `»~»`) are unaffected.
The marker/base-op spelling code moved to its own
`precedence_meta_ops/hyper_spelling.rs` to keep `hyper_concat.rs` under the
500-line limit.

Pinned by `t/lang/operators/user-infix-hyper-marker-spelling.t`. OneSeq's test
file now parses; 17 of its 21 assertions still fail on a separate run-time gap
in its dependency ForwardIterables (`Iterator.push-all` into an
`IterationBuffer` writes nothing), filed as #9419.
