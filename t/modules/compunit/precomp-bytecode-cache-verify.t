use Test;

# ADR-11756 step 4: with MUTSU_PRECOMP_BYTECODE=1 a module's mainline compile is
# written to the precompilation cache and served to the next process. With
# MUTSU_PRECOMP_VERIFY=1 a hit is compiled afresh as well and the two encodings
# must be byte-identical, or the run stops with status 70. So the warm run below
# proves that the cached compile is exactly what this process would have built,
# for a module holding every construct that once differed between processes.
plan 6;

my $lib = $?FILE.IO.parent(2).sibling('lib').Str;
my $cache = $*TMPDIR.add("mutsu-bytecode-precomp-{$*PID}");
my $script = $cache.add('probe.raku');

$cache.mkdir;
$script.spurt: qq:to/PROBE/;
    use lib '$lib';
    use PrecompBytecodeProbe;
    say probe();
    PROBE

sub run-probe() {
    my %env = %*ENV;
    %env<XDG_CACHE_HOME> = $cache.Str;
    %env<MUTSU_PRECOMP_BYTECODE> = '1';
    %env<MUTSU_PRECOMP_VERIFY> = '1';
    %env<MUTSU_PRECOMP_TRACE> = '1';
    my $proc = run($*EXECUTABLE, $script.Str, :out, :err, :%env);
    my $out = $proc.out.slurp(:close).chomp;
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err)
}

my $expected = '1|2|True|False|42|hi from Ann';

my ($cold-code, $cold-out, $cold-err) = run-probe();
is $cold-out, $expected, 'cold run: the module answers correctly'
    or diag $cold-err;
like $cold-err, /'PrecompBytecodeProbe.rakumod: compiled and cached'/,
    'cold run: the mainline compile is written to the cache';

my ($warm-code, $warm-out, $warm-err) = run-probe();
is $warm-code, 0, 'warm run: the cached compile verifies against a fresh one'
    or diag $warm-err;
like $warm-err, /'PrecompBytecodeProbe.rakumod: hit'/,
    'warm run: the mainline is served from the cache';
is $warm-out, $expected, 'warm run: the cached compile gives the same answers';
is $cold-code, 0, 'cold run exits cleanly';

END {
    try $cache.&rmtree if $cache.e;
}

sub rmtree(IO::Path $d) {
    for $d.dir -> $e { $e.d ?? rmtree($e) !! $e.unlink }
    $d.rmdir;
}
