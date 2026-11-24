use Test;

# The static-outer latch (#18787) is process-global: it is set by the first
# unit a process reads that mentions a dynamic neighbour's lexical chain
# (`$CALLER::OUTER::x`). Under it, a sub declared below its unit's top level
# carries a static view of its declaring scope, so the latch is an input of a
# module's compile. A module cached by a process that never set it must not be
# served to one that did, and the other way round (#19166). Pinned end to end
# with MUTSU_PRECOMP_VERIFY, which exits 70 when a served entry differs from a
# fresh compile.

plan 8;

my $dir = $*TMPDIR.add("mutsu-static-outer-latch-input-{$*PID}");
my $lib = $dir.add('lib');
$lib.mkdir;

# A cache root of its own, so the first run really is cold.
%*ENV<XDG_CACHE_HOME> = $dir.add('cache').Str;
%*ENV<MUTSU_PRECOMP_VERIFY> = '1';

$lib.add('StaticOuterLatchMod.rakumod').spurt: q:to/MOD/;
    unit module StaticOuterLatchMod;
    sub latch-probe() is export {
        my $seen = 41;
        my sub inner() { $seen + 1 }
        inner();
    }
    MOD

my $plain = $dir.add('plain.raku');
$plain.spurt: q:to/PROG/;
    use StaticOuterLatchMod;
    say latch-probe();
    PROG

# The mention alone sets the latch before the module is loaded.
my $latched = $dir.add('latched.raku');
$latched.spurt: q:to/PROG/;
    # reads $CALLER::OUTER::x nowhere; the mention is enough to latch
    use StaticOuterLatchMod;
    say latch-probe();
    PROG

sub run-script(IO::Path $script) {
    my $proc = run($*EXECUTABLE, '-I', $lib.Str, $script.Str, :out, :err);
    my $out = $proc.out.slurp;
    my $err = $proc.err.slurp;
    diag $err if $proc.exitcode;
    %( :$out, exit => $proc.exitcode )
}

for (
    $plain,   'unlatched, cold cache',
    $latched, 'latched, after an unlatched compile',
    $latched, 'latched, warm',
    $plain,   'unlatched, after a latched compile',
) -> $script, $what {
    my %r = run-script($script);
    is %r<exit>, 0, "$what: the served entry matches a fresh compile";
    is %r<out>, "42\n", "$what: the module runs";
}

END {
    try rmtree($dir) if $dir.e;
}

sub rmtree(IO::Path $d) {
    for $d.dir -> $e {
        $e.d ?? rmtree($e) !! $e.unlink;
    }
    $d.rmdir;
}
