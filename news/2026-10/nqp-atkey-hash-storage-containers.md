# `nqp::atkey` on a Hash storage hands out the value's container

Rakudo's `Hash` keeps Scalar containers in its `$!storage`, and code such as
Hash2Class tells "not yet converted" from "converted" with
`nqp::iscont(nqp::atkey($storage, $key))`, replacing the slot with
`nqp::bindkey`. mutsu answered the bare value, so the test was always false and
nothing was ever converted.

`atkey` now returns the shared element cell (the twin of `atpos` on `$!reified`)
for a Hash's storage, the first `atkey` promotes every value once (a flag, no size growth) so a value `bindkey` replaced stays bare,
and a raw `nqp::hash` or a `Map` still hold bare values. `nqp::iscont(my $v :=
EXPR)` now asks about the declared variable's binding instead of the decontainerized
value. Fixes #15106.
