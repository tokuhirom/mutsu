# ADR-8673 D2: a small statically linked TRIR callee is spliced into its
# caller instead of being called.
#
# The fixture covers every shape the splice has to keep, one per line:
# - a native `is rw` parameter, and one passed on;
# - evaluated arguments on both banks;
# - a read-only parameter the callee writes;
# - the callee's free variable, rebound by a call it makes;
# - a `fail` that becomes the callee's Failure value;
# - an exception;
# - `callframe` from inside;
# - a `.wrap` installed after linking;
# - an early `return` from a loop.
# The transcript must be the same with inlining on, off
# (`MUTSU_TRIR_INLINE=0`) and with TRIR off, and is pinned against rakudo's.
use Test;

plan 7;

my $fixture = $?FILE.IO.parent(3).add('fixtures/trir-inline.raku').Str;

sub transcript(%extra-env) {
    my %env = %*ENV;
    %env{$_} = %extra-env{$_} for %extra-env.keys;
    my $proc = run($*EXECUTABLE, $fixture, :out, :err, :%env);
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err)
}

my ($on-code, $on-out, $on-err) = transcript({ MUTSU_VM_STATS => '1' });
my ($no-code, $no-out, $no-err) = transcript({ MUTSU_TRIR_INLINE => '0', MUTSU_VM_STATS => '1' });
my ($off-code, $off-out, $off-err) = transcript({ MUTSU_TRIR => 'off' });

is $on-code, 0, 'the fixture runs clean with inlining'
    or diag "stderr was:\n$on-err";
is $no-out, $on-out, 'inlining off agrees';
is $off-out, $on-out, 'TRIR off agrees';

# Checked against rakudo 2026.11, except `after-wrap`: rakudo's own static
# inliner has already inlined `small` into `use-small` and misses a wrapper
# installed later (2,3); mutsu honours it, with and without inlining, as its
# untyped path does.
is $on-out, q:to/END/, 'the transcript';
    rw=13
    rw-ref=7
    values=853
    copy=101
    outer=100 then 180
    fail=ok 3 / failed: nonpositive -1
    die=caught: too big 9 2
    frames=inner-site<outer-site
    before-wrap=2,3
    after-wrap=20,30
    early-return=2,-1,0
    END

# Not vacuous: regions were entered with inlining on, none with it off.
sub stat($err, $key) {
    my $line = $err.lines.first(*.contains('trir: entries=')) // '';
    +($line ~~ / "$key=" (\d+) /)[0]
}
ok stat($on-err, 'inlined') > 0, 'callees were inlined (' ~ stat($on-err, 'inlined') ~ ' regions)';
is stat($no-err, 'inlined'), 0, 'MUTSU_TRIR_INLINE=0 inlines nothing';
is stat($on-err, 'bails'), 0, 'no inlined region bailed';
