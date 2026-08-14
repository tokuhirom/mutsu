# `nqp::add_n`/`sub_n`/`mul_n`/`div_n`/`neg_n`/`abs_n` were missing

Triaging the dist-test-suite-failures batch's un-triaged `Random::Choice`
row (`todo/tickets/dist-test-suite-failures-batch.md`) turned up a gap in
the `nqp::` op table: native `num` (floating point) arithmetic ops were
entirely unimplemented, even though the native `int` family (`add_i`,
`sub_i`, `mul_i`, ...) and the native `num` *comparisons* (`iseq_n`,
`islt_n`, ...) already existed in `src/runtime/nqp_ops.rs`.

`Random::Choice`'s alias-method sampler is written directly against these
ops (`nqp::mul_n(nqp::div_n(Num($x), $total), $!n)`), so it died with
"Unsupported nqp:: op: nqp::div_n" before running a single test. Added the
missing six ops generally, matching the existing `_i` pattern. Pinned by
`t/nqp-native-num-arith.t`. `Random::Choice`'s own test suite now passes
6/6, matching raku.
