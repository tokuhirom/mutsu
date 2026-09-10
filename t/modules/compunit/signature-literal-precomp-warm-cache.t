use Test;

# A `Signature` literal (`:(Routine, :$native!)`) is an `Expr::Literal` holding a
# `Signature` instance, but its structured parameter data lives in a
# process-global side table keyed by that instance's id -- which nothing in a
# serialized value can reach. A module restored from the precompilation cache
# therefore came back with a literal whose id named nothing, `extract_sig_info`
# fell through to its empty-params legacy shape, and `Signature ~~ Signature`
# went from True on a cold run to False on a warm one.
#
# Only the SECOND run of a program sees it, which is exactly what CI's always-cold
# runners cannot catch -- so pin it here, the way `t/precomp-warm-cache-parity.t`
# does for the other replay effects.
plan 3;

my $lib = $?FILE.IO.parent.add('lib').Str;
my $cache = $*TMPDIR.add("mutsu-sig-precomp-{$*PID}");
my $script = $cache.add('probe.raku');

$cache.mkdir;
$script.spurt: qq:to/PROBE/;
    use lib '$lib';
    use SignatureLiteralProbe;
    say signature-probe();
    PROBE

# Our own cache directory, so the first run really is cold whatever the
# developer's ~/.cache holds.
%*ENV<XDG_CACHE_HOME> = $cache.Str;

sub run-probe() {
    my $proc = run($*EXECUTABLE, $script.Str, :out, :err);
    $proc.err.slurp;
    $proc.out.slurp.chomp;
}

my $cold = run-probe();
my $warm = run-probe();

is $cold, 'True,False',
    'cold run: the signature literal matches exactly the one candidate it names';
is $warm, $cold,
    'warm run from the precompilation cache gives the identical answer';
is $warm, 'True,False',
    'the restored signature literal still carries its parameters';

END {
    try $cache.&rmtree if $cache.e;
}

sub rmtree(IO::Path $d) {
    for $d.dir -> $e {
        $e.d ?? rmtree($e) !! $e.unlink;
    }
    $d.rmdir;
}
