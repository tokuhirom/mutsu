use Test;

# `lives-ok`/`dies-ok` run their block through a nested-run boundary
# (`eval_block_value`/`with_nested_registers`). A `return` executed inside
# that block is NOT necessarily "the block died": when the block calls a
# closure that was captured *outside* lives-ok/dies-ok (e.g. by the routine
# that also called lives-ok/dies-ok), the `return` targets that outer,
# still-live routine call frame and must keep propagating past lives-ok/
# dies-ok's own pass/fail reporting -- exactly like Rakudo, where the
# enclosing routine returns immediately and the lives-ok/dies-ok assertion is
# never recorded at all (verified against `raku`).
#
# This is the "direction A" counterpart to
# t/tap-callback-nonlocal-return.t and to
# todo/deep/return-outside-routine-uncatchable-inside-nested-run.md's
# direction (1): a `return` meant for a live outer VM frame reached across a
# nested-run boundary must NOT be treated as a failure locally.
plan 2;

sub outer-return() {
    my $cb = -> { return "early" };
    lives-ok { $cb() }, "cb lives (return) -- should never run: the return escapes first";
    return "late";
}
is outer-return(), "early",
    'return inside a lives-ok block targeting the calling sub skips the assertion and returns from the sub';

sub outer-dies-ok() {
    my $cb = -> { return "early2" };
    dies-ok { $cb() }, "cb dies (return) -- should never run: the return escapes first";
    return "late2";
}
is outer-dies-ok(), "early2",
    'return inside a dies-ok block targeting the calling sub skips the assertion and returns from the sub';
