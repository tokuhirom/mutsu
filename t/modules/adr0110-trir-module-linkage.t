# #9072: ADR-0110's statically linked calls inside a MODULE.
#
# Two defects hid each other. A linked callee (`CallTr`) was looked up at run
# time in the table the running frame had in hand, which for a module routine
# is its own nested-sub table, so every linked call bailed and the routine
# re-ran untyped. And once linked, an expression-position `(return @out)`
# (JSON::Fast's loop exit) escaped the linked frame and returned from the
# outermost untyped call instead, with the inner value and an unwritten
# `$pos`: `[[1]]` parsed as `[1]` at position 3. A third made the linked
# callee's free `my` (a `module { }` block lexical) unresolvable.
#
# So the test pins both halves: the answers (TRIR on == TRIR off == the
# hand-checked transcript), and that TRIR actually ran them to completion
# rather than bailing back to the untyped path, which is what would make the
# answers agree vacuously.
use Test;

plan 6;

my $fixture = $?FILE.IO.parent(2).add('fixtures/trir-module-linkage.raku').Str;
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

my ($on-code, $on-out, $on-err) = transcript({ MUTSU_VM_STATS => '1' });
my ($off-code, $off-out, $off-err) = transcript({ MUTSU_TRIR => 'off' });

is $on-code, 0, 'the fixture runs clean with TRIR on'
    or diag "stderr was:\n$on-err";
is $off-code, 0, 'the fixture runs clean with TRIR off'
    or diag "stderr was:\n$off-err";
is $on-out, $off-out, 'TRIR and the untyped path agree';

is $on-out, q:to/END/, 'the transcript carries the expected answers';
    5 => 5 pos=1
    [1] => $[1] pos=3
    [[1]] => $[[1],] pos=5
    [[1],2] => $[[1], 2] pos=7
    [ [1 , [2]], 3 ] => $[[1, [2]], 3] pos=16
    [[[7]],[8,9]] => $[[[7],], [8, 9]] pos=13
    count-ws => 6
    END

my $stats = $on-err.lines.first(*.contains('] trir: entries=')) // '';
my %n = $stats.comb(/ <[\w-]>+ '=' \d+ /).map({ .split('=') }).map({ .[0] => .[1].Int });
ok %n<completed> > 0 && %n<bails> == 0,
    'the module routines ran in TRIR to completion, with no bail'
    or diag "trir stats line was: '$stats'";
