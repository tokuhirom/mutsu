# ADR-0110's differential gate: a routine compiled to the typed, resolved IR
# must answer EXACTLY what the same routine answers on the untyped path —
# same values, same exception types, same `is rw` visibility — for every
# signature shape TRIR admits and every boundary it must decline to.
#
# `MUTSU_TRIR=off` forces every routine to decline, so running one fixture
# both ways and requiring the two transcripts to be byte-identical is the
# whole test. It is also the A/B measurement switch (ADR-0110 §5).
#
# The shapes themselves live in t/fixtures/trir-shapes.raku so that adding one
# needs no change here.
use Test;

plan 5;

my $fixture = $?FILE.IO.parent(3).add('fixtures/trir-shapes.raku').Str;
ok $fixture.IO.e, "fixture is where this test expects it ($fixture)";

sub transcript(%extra-env) {
    # A hash built by assignment, not `:env(%*ENV, |%extra-env)`: mutsu drops
    # the extra pairs of that list form (#9085), so the "off" run silently
    # ran with TRIR on and the comparison was vacuous.
    my %env = %*ENV;
    %env{$_} = %extra-env{$_} for %extra-env.keys;
    my $proc = run($*EXECUTABLE, $fixture, :out, :err, :%env);
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err)
}

my ($on-code, $on-out, $on-err) = transcript({});
my ($off-code, $off-out, $off-err) = transcript({ MUTSU_TRIR => 'off' });

is $on-code, 0, 'the fixture runs clean with TRIR on'
    or diag "stderr was:\n$on-err";
is $off-code, 0, 'the fixture runs clean with TRIR off'
    or diag "stderr was:\n$off-err";

is $on-out, $off-out, 'TRIR and the untyped path agree on every shape';

# Not vacuous: the fixture has to have actually produced the answers, so pin a
# few of them against hand-checked expectations rather than only against each
# other. `nom-ws-from-1=5` is the scanner shape ADR-0110 §7 Stage 1 names.
my %got = $on-out.lines.map({ .split('=', 2) }).map({ .[0] => .[1] });
is-deeply
    %got<nom-ws-from-1 nom-ws-past-end arith classify wrap rw-through-cell>.List,
    ('5', '99', '25', '-1,0,1', '-2', '5,5').List,
    'the transcript carries the expected answers, not an empty agreement';
