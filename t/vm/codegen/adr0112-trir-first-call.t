# ADR-0112 Step 3: a TRIR routine runs its chunk on the call that resolves
# its name, not only on later resolution-cache hits.
#
# Every routine in the fixture is called from untyped code, and most of them
# exactly once. Before this, such a call ran the untyped body: a routine
# called once never ran typed at all, and a recursive-descent parser's
# outermost call (JSON::Fast's top-level `parse-array`) ran its whole loop
# untyped. The fixture covers a single call, an `is rw` native written back on
# the resolving call, a loop that calls a TRIR routine from untyped code, and
# the two shapes that must still take the untyped path: a named argument and a
# `.wrap`ped routine.
#
# Pinned: TRIR on == TRIR off == the transcript (checked against rakudo), and
# the `trir:` stats line shows every one of those calls entered the chunk.
use Test;

plan 6;

my $fixture = $?FILE.IO.parent(3).add('fixtures/trir-first-call.raku').Str;

sub transcript(%extra-env) {
    my %env = %*ENV;
    %env{$_} = %extra-env{$_} for %extra-env.keys;
    my $proc = run($*EXECUTABLE, $fixture, :out, :err, :%env);
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err)
}

my ($on-code, $on-out, $on-err) = transcript({ MUTSU_VM_STATS => '1' });
my ($off-code, $off-out, $off-err) = transcript({ MUTSU_TRIR => 'off' });

is $on-code, 0, 'the fixture runs clean with TRIR on'
    or diag "stderr was:\n$on-err";
is $off-code, 0, 'the fixture runs clean with TRIR off'
    or diag "stderr was:\n$off-err";
is $on-out, $off-out, 'TRIR and the untyped path agree';

is $on-out, q:to/END/, 'the transcript carries the expected answers';
    spin => 499500
    advance => 4 4
    nest => 6 9
    named => rejected
    wrapped => 41
    END

# `spin` and `advance` are called once each and `depth` three times, all from
# untyped code: five entries. Only the two later `depth` calls entered before.
my $stats = $on-err.lines.first(*.contains('trir: entries='));
ok $stats, 'the run reports its trir: line' or diag $on-err;
my %n = ($stats // '').match(/(<[\w-]>+) '=' (\d+)/, :g).map(-> $m { ~$m[0] => +$m[1] });
ok %n<entries> >= 5 && %n<completed> == %n<entries> && %n<bails> == 0,
    'every resolving call entered its chunk and completed'
    or diag $stats;
