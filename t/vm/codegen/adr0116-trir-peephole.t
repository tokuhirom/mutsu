# The TRIR peephole pass (ADR-0116 §7): compare-and-branch fusion, jump
# threading of `&&` / `||` keep jumps, dead pushes, and `nqp::if` compiled for
# effect in sink position, each pinned against the untyped path and rakudo.
#
# The fixture runs every fused shape over inputs that take both sides of each
# branch: a slot against a constant for all six comparisons under `nqp::if`
# and `nqp::unless` (and a constant too wide for the fused operand), computed
# operands, `||` / `&&` chains both as a branch condition and as a value,
# sink-position `nqp::if` arms of different kinds, and the
# `unjsonify-string` drain loop (`elems` / `shift_i` / `push_i`), whose body
# fuses to slot-to-slot list ops and whose back edge is rotated onto the
# emptiness test, once plain and once through a wrapping `uint8`.
#
# Pinned: TRIR on == TRIR off == the transcript (checked against rakudo), the
# routines are accepted, and the chunks really contain the fused ops, so the
# agreement is not vacuous.
use Test;

plan 6;

my $fixture = $?FILE.IO.parent(3).add('fixtures/trir-peephole.raku').Str;

sub transcript(%extra-env) {
    my %env = %*ENV;
    %env{$_} = %extra-env{$_} for %extra-env.keys;
    my $proc = run($*EXECUTABLE, $fixture, :out, :err, :%env);
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err)
}

my ($on-code, $on-out, $on-err) = transcript({ MUTSU_TRIR_DUMP => 'ops' });
my ($off-code, $off-out, $off-err) = transcript({ MUTSU_TRIR => 'off' });

is $on-code, 0, 'the fixture runs clean with TRIR on'
    or diag "stderr was:\n$on-err";
is $off-code, 0, 'the fixture runs clean with TRIR off'
    or diag "stderr was:\n$off-err";
is $on-out, $off-out, 'TRIR and the untyped path agree';

is $on-out, q:to/END/, 'the transcript carries the expected answers';
    -4:126
    -3:122
    0:114
    5:113
    6:114
    7:82
    8:66
    3/0
    3/3
    1/3
    0/2
    0:0:0:42
    1:6:101:1
    2:2:102:2
    5:2:105:5
    9:3:109:9
    10:1:110:10
    11:0:111:11
    11/0
    1/2
    0/3
    0/3
    ab c de


    250,251,252,253,254,255,0,1,2
    END

my @routines = <cmp-slot cmp-computed chains sink-arms drain drain-u8>;
my @accepted = $on-err.lines.map({ m/^ 'trir: ' (\S+) ' accepted'/ ?? ~$0 !! Empty }).grep(* (elem) @routines);
is-deeply @accepted.sort.List, @routines.sort.List, 'every shape routine is accepted into TRIR'
    or diag $on-err;

my @fused = <JumpCmpLC JumpCmpC JumpCmp JumpIfEmptyLocal PushILocalVoid
    JumpIfNonEmptyLocal ShiftIStoreLocal PushISlotLocalVoid>;
my @seen = @fused.grep(-> $op { $on-err.contains($op ~ ' {') || $on-err.contains($op ~ '(') });
is-deeply @seen.List, @fused.List, 'the chunks carry every fused op form'
    or diag $on-err;
