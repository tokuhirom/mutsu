# ADR-0116 D2.4: op-body costs removed from TRIR's hot paths, each pinned
# against the untyped path and rakudo.
#
# - A free variable read out of a `package P { my ... }` block's lexical store
#   (JSON::Fast's `$ws`) is cached with the chunk's other free variables, so
#   an assignment made by another routine must still be seen on the next call.
# - A `Uni:D` parameter check accepts every normalization form without the
#   general type checker, and a form constraint still rejects the others.
# - `nqp::findcclass` / `findnotcclass` scan an ASCII string byte by byte,
#   with the same window rules as the grapheme walk they skip.
#
# Pinned: TRIR on == TRIR off == the transcript (checked against rakudo),
# and the routines are actually accepted, so the agreement is not vacuous.
use Test;

plan 5;

my $fixture = $?FILE.IO.parent(3).add('fixtures/trir-op-bodies.raku').Str;

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
    probe => 10 30
    probe => 20
    reassigned => 70 90
    reassigned => 80
    uni => 97 101 65 120
    nfd => 2 rejected
    scan => 6/6 5/5 8/10 3/3 2/2 2/2
    scan => 6/6 5/5 8/10 3/3 2/2 2/2
    END

my @routines = <probe first-code nfd-only scan>;
my @accepted = $on-err.lines.map({ m/^ 'trir: ' (\S+) ' accepted'/ ?? ~$0 !! Empty }).grep(* (elem) @routines);
is-deeply @accepted.sort.List, @routines.sort.List, 'every shape routine is accepted into TRIR'
    or diag $on-err;
