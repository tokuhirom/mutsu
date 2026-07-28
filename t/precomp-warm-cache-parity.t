use Test;

# A precompilation cache hit skips the module's parse entirely. That is only
# sound if parsing is a pure `source -> AST` function, and it is not: the parser
# also leaves state behind that the runtime reads afterwards. Whatever a cache
# hit fails to reproduce turns into a bug that only shows up on the *second* run
# of a program -- invisible to CI, whose runners always start cold.
#
# So the contract this pins is simply: running the same program twice, the
# second time with a warm cache, must produce byte-identical output.
#
# The probe module (t/lib/PrecompRevisionProbe.rakumod) exercises the two
# effects measured to matter:
#   - it is `use v6.e.PREVIEW`, and computes `sprintf('%#x', -256)` in its
#     mainline. 6.e puts the sign before the radix prefix (`-0x100`); 6.d gives
#     `0x-100`. Without the replay the warm run compiled it under the importer's
#     revision and printed `0x-100`.
#   - it declares a duplicated `is export` trait, so its parse emits a warning.
#     Without the replay the warning appeared on the cold run and vanished after.

plan 5;

my $lib = $?FILE.IO.parent.add('lib').Str;
my $cache = $*TMPDIR.add("mutsu-precomp-parity-{$*PID}");
my $script = $cache.add('probe.raku');

$cache.mkdir;
$script.spurt: qq:to/PROBE/;
    use lib '$lib';
    use PrecompRevisionProbe;
    say \$revision-probe;
    say precomp-probe-hello();
    PROBE

# Point the cache at an empty directory of our own so the run really is cold,
# whatever the developer's ~/.cache happens to hold.
%*ENV<XDG_CACHE_HOME> = $cache.Str;

sub run-probe() {
    my $proc = run($*EXECUTABLE, $script.Str, :out, :err);
    my $out = $proc.out.slurp;
    my $err = $proc.err.slurp;
    return %( :$out, :$err );
}

my %cold = run-probe();
my %warm = run-probe();

ok %cold<out>.contains('-0x100'),
    'cold run compiles the module under its own 6.e revision';
is %warm<out>, %cold<out>,
    'stdout is identical with a warm cache';
ok %warm<out>.contains('-0x100'),
    'the warm run still reports the 6.e revision (replayed from the cache)';
ok %cold<err>.contains('Duplicate'),
    'cold run reports the parse warning';
is %warm<err>, %cold<err>,
    'stderr is identical with a warm cache';

END {
    # `run` may still hold the directory; ignore a failed cleanup.
    try $cache.&rmtree if $cache.e;
}

sub rmtree(IO::Path $d) {
    for $d.dir -> $e {
        $e.d ?? rmtree($e) !! $e.unlink;
    }
    $d.rmdir;
}
