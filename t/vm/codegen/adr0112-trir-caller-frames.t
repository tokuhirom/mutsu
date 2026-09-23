# A TRIR routine's body runs under its own `routine_stack` frame.
#
# `CALLER::` components count routine frames and are validated against
# `routine_stack`, which a TRIR frame used not to appear on. An untyped callee
# of a TRIR routine therefore saw one frame fewer than there were:
# `CALLER::CALLER::.BIND-KEY` from two calls down died with "frame is gone".
# It surfaced when the resolving call started entering TRIR too
# (tests/stash_bind_key.rs), but a resolution-cache hit had the same gap, so
# the fixture calls the routine twice.
#
# Pinned: TRIR on == TRIR off == the transcript (checked against rakudo), and
# the routine in between is accepted into TRIR.
use Test;

plan 5;

my $fixture = $?FILE.IO.parent(3).add('fixtures/trir-caller-frames.raku').Str;

sub transcript(%extra-env) {
    my %env = %*ENV;
    %env{$_} = %extra-env{$_} for %extra-env.keys;
    my $proc = run($*EXECUTABLE, $fixture, :out, :err, :%env);
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err)
}

my ($on-code, $on-out, $on-err) = transcript({ MUTSU_TRIR_DUMP => '1' });
my ($off-code, $off-out, $off-err) = transcript({ MUTSU_TRIR => 'off' });

is $on-code, 0, 'the fixture runs clean with TRIR on'
    or diag "stderr was:\n$on-err";
is $off-code, 0, 'the fixture runs clean with TRIR off'
    or diag "stderr was:\n$off-err";
is $on-out, $off-out, 'TRIR and the untyped path agree';
is $on-out, q:to/END/, 'the transcript carries the expected answers';
    bound => 41 41
    stored => 73
    END
ok $on-err.lines.first(* eq 'trir: install accepted (2 ops, 0 native slots, 1 obj slots, 0 outers, 1 calls)'),
    'the routine in between runs as TRIR'
    or diag $on-err;
