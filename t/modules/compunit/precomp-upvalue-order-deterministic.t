use Test;

# #12606: the order of a closure's `upvalue_syms` (free `&name`s it only hands
# on to nested closures) came from a HashSet, so two processes compiled
# Test::Tap differently and the warm run under MUTSU_PRECOMP_VERIFY=1 stopped
# with status 70. Cold and warm runs are separate processes with separate hash
# seeds; several pairs make a regression all but certain to show.
plan 6;

my $helpers = $?FILE.IO.parent(3).sibling('roast').add('packages/Test-Helpers/lib').Str;
my $cache = $*TMPDIR.add("mutsu-upvalue-order-{$*PID}");
$cache.mkdir;
my $script = $cache.add('probe.raku');
$script.spurt: q:to/PROBE/;
    use Test; use Test::Tap;
    plan 1;
    tap-ok Supply.from-list(1, 2), [1, 2], 'tap works';
    PROBE

for ^3 -> $i {
    my %env = %*ENV;
    %env<XDG_CACHE_HOME> = $cache.add("c$i").Str;
    %env<MUTSU_PRECOMP_VERIFY> = '1';
    for <cold warm> -> $phase {
        my $proc = run($*EXECUTABLE, '-I', $helpers, $script.Str, :out, :err, :%env);
        $proc.out.slurp(:close);
        my $err = $proc.err.slurp(:close);
        is $proc.exitcode, 0, "round $i $phase run verifies" or diag $err;
    }
}

LEAVE { run 'rm', '-rf', $cache.Str }
