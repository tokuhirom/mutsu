# `nqp::with`, `nqp::without` and `nqp::defor`

The definedness-testing `nqp::` control forms now compile to jumps, like
`nqp::if` and `nqp::ifnull` (#11500, part of the `nqp::` coverage campaign
#11488). Their branches are thunks: only the selected one is evaluated, and the
test is Raku's `.defined`, so a `Failure` takes the undefined arm. As in Rakudo,
a missing `else` yields the tested value itself (`nqp::with(Any, 1)` is `Any`),
a block operand is yielded rather than called, and `nqp::defor($a, $b)` is
`$a // $b`. TRIR compiles `defor` with the same lowering as `ifnull`.

`nqp::for` is recorded under "Not applicable" in `docs/nqp-op-coverage.md`:
Rakudo rejects every Raku call to it at compile time, because a block literal
never reaches the op as the bare `QAST::Block` it requires (NQP's own
`nqp::for` fails the same way). The Conditional and Loop/Control categories are
now complete.
