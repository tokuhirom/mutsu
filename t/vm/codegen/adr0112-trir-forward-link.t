# ADR-0112 Step 1: a TRIR routine's call to a routine declared AFTER it.
#
# Such a call is a `CallGen` (the compile has not seen the callee yet). It
# used to go through the whole by-name dispatch every time and run the
# callee untyped: 8.9 µs a call against 172 ns for a linked one. It is now
# linked at run time to the chunk the generic dispatch actually reached
# (`src/trir/gen_link.rs`). The test pins the answers (TRIR on == TRIR off ==
# the hand-checked transcript) for the shapes the link must get right:
#
# - a native `is rw` write through the forward call;
# - mutual recursion, where one direction is necessarily forward;
# - an aggregate argument, which must decline to the generic path, followed
#   by a scalar one, which must not stay declined;
# - a `.wrap` installed AFTER the site linked, which must still be reached.
#   (rakudo prints 2,3 there because its optimizer resolved the call at
#   compile time without `use soft`; mutsu's untyped path reaches the
#   wrapper, and the link must not change what mutsu answers.)
#
# It also pins that the link is actually used (`gen-links` > 0), so the
# agreement cannot be vacuous.
use Test;

plan 5;

my $fixture = $?FILE.IO.parent(3).add('fixtures/trir-forward-link.raku').Str;

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
    bump-twice => 12 p=12
    bump-twice => 12 p=12
    bump-twice => 12 p=12
    is-even(10) => 1
    is-even(7) => 0
    is-even(10) again => 1
    describe => Int,Str,Array,Int
    before wrap => 2,3,4
    after wrap => 200,300
    END

my $stats = $on-err.lines.first(*.contains('] trir: entries=')) // '';
my %n = $stats.comb(/ <[\w-]>+ '=' \d+ /).map({ .split('=') }).map({ .[0] => .[1].Int });
ok (%n<gen-links> // 0) > 0 && %n<bails> == 0,
    'forward calls ran through their link, with no bail'
    or diag "trir stats line was: '$stats'";
