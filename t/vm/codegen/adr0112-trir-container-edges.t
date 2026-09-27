# #9122: the per-element work of JSON::Fast's `parse-array` -- a fresh
# IterationBuffer installed as an Array's `$!reified` and filled afterwards,
# which now re-points the buffer without vivifying a store for it -- and
# `nqp::ordat` reading one text over and over and then switching texts, which
# TRIR now answers from a one-entry grapheme-index memo.
#
# Pinned: TRIR on == TRIR off == the transcript (checked against rakudo), and
# the TRIR-shaped routines are actually accepted, so the agreement is not
# vacuous.
use Test;

plan 5;

my $fixture = $?FILE.IO.parent(3).add('fixtures/trir-container-edges.raku').Str;

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

# The fixture runs its calls twice.
is $on-out, q:to/END/ x 2, 'the transcript carries the expected answers';
    fresh-then-push => 4:0,10,20,30
    fresh-over-filled => 1:9
    filled-then-bind => 3:a,b,c
    alternate => 97,233,98,128512,99,122
    alternate-again => 120,97,121,98
    edges => 104 -1 -1
    END

my @routines = <fresh-then-push filled-then-bind alternate edges>;
my @accepted = $on-err.lines.map({ m/^ 'trir: ' (\S+) ' accepted'/ ?? ~$0 !! Empty }).grep(* (elem) @routines);
is-deeply @accepted.sort.List, @routines.sort.List, 'every TRIR-shaped routine is accepted'
    or diag $on-err;
