# ADR-0112 Step 3: the typed list ops TRIR runs without the `nqp::` dispatch
# table -- `nqp::elems` answered on the native bank, `nqp::shift_i` answered
# on the native bank, `nqp::push_i` of a native.
#
# The fixture has one routine per shape: `elems` of a Uni, a list, a hash
# (handed on to the generic op) and an IterationBuffer; `shift_i` into a
# native, a sized native that wraps, and an expression; `push_i` of a native
# and of a boxed value; and the consume-from-the-front loop JSON::Fast's
# `unjsonify-string` is.
#
# Pinned: TRIR on == TRIR off == the transcript (checked against rakudo),
# and every routine is actually accepted, so the agreement is not vacuous.
use Test;

plan 5;

my $fixture = $?FILE.IO.parent(3).add('fixtures/trir-list-ops.raku').Str;

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
    counts => 6 3 2 1
    drain-sized => 4294967295 44 7 0
    build => abcF 70 4
    copy-except => abc 0
    END

my @routines = <counts drain-sized build copy-except>;
my @accepted = $on-err.lines.map({ m/^ 'trir: ' (\S+) ' accepted'/ ?? ~$0 !! Empty }).grep(* (elem) @routines);
is-deeply @accepted.sort.List, @routines.sort.List, 'every shape routine is accepted into TRIR'
    or diag $on-err;
