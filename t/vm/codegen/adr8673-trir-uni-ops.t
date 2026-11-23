# Native-int list ops on a `Uni` (`push_i`, `shift_i`, `elems`) and the
# generic `nqp::` data ops a TRIR body runs with its routine-stack entries
# still deferred (ADR-8673 D1). The `Uni` store is recognized by its tag and
# the data ops by a per-op-id table; the fixture's transcript must be the
# same with TRIR on and off, and it is pinned against rakudo's.
use Test;

plan 4;

my $fixture = $?FILE.IO.parent(3).add('fixtures/trir-uni-ops.raku').Str;

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
is $off-out, $on-out, 'TRIR off agrees';

# Checked against rakudo 2026.11.
is $on-out, q:to/END/, 'the transcript is what rakudo prints';
    filled=5 65,66,67,68,69
    drained=62061635 left=0
    bound=3 72480 left=0
    buffer=7
    ints=4 3078 left=0
    END

# Not vacuous: every routine of the fixture ran as TRIR.
is $on-err.lines.grep(*.contains(' accepted ')).elems, 7,
    'every routine of the fixture compiled to TRIR';
