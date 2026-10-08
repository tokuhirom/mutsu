use Test;

# ADR-12026 §2.4: a precompiled module records where its Pod blocks are, and a
# hit rebuilds `$=pod` from those ranges alone instead of scanning the whole
# source (read at the module's load: mutsu's `$=pod` is not visible from
# its subs later on). The module mixes Pod with a heredoc that only looks like Pod, so
# the warm run must agree with the cold run (MUTSU_PRECOMP_VERIFY=1 also
# recomputes the recorded ranges and stops with status 70 on a difference).
plan 6;

my $lib = $?FILE.IO.parent(2).sibling('lib').Str;
my $cache = $*TMPDIR.add("mutsu-pod-ranges-{$*PID}");
my $script = $cache.add('probe.raku');

$cache.mkdir;
$script.spurt: qq:to/PROBE/;
    use lib '$lib';
    use PrecompPodRangesProbe;
    say \$PrecompPodRangesProbe::COUNT ~ "|" ~ \$PrecompPodRangesProbe::LINES;
    say \$PrecompPodRangesProbe::KINDS;
    PROBE

sub run-probe() {
    my %env = %*ENV;
    %env<XDG_CACHE_HOME> = $cache.Str;
    %env<MUTSU_PRECOMP_BYTECODE>:delete;
    %env<MUTSU_PRECOMP_VERIFY> = '1';
    %env<MUTSU_PRECOMP_TRACE> = '1';
    my $proc = run($*EXECUTABLE, $script.Str, :out, :err, :%env);
    my $out = $proc.out.slurp(:close).chomp;
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err)
}

my ($cold-code, $cold-out, $cold-err) = run-probe();
is $cold-code, 0, 'cold run exits cleanly' or diag $cold-err;
like $cold-out, /^ '4|3' \n .+ /, 'cold run: four Pod blocks, heredoc not read as Pod';

my ($warm-code, $warm-out, $warm-err) = run-probe();
is $warm-code, 0, 'warm run: recorded Pod ranges verify' or diag $warm-err;
like $warm-err, /'PrecompPodRangesProbe.rakumod: hit'/,
    'warm run: the mainline is served from the cache';
is $warm-out, $cold-out, 'warm run: $=pod is the same as on the cold run';
is $warm-out.lines[1], 'Pod::Block::Named,Pod::Block::Comment,Pod::Heading,Pod::Block::Table',
    'warm run: the Pod blocks, in order';

END {
    try $cache.&rmtree if $cache.e;
}

sub rmtree(IO::Path $d) {
    for $d.dir -> $e { $e.d ?? rmtree($e) !! $e.unlink }
    $d.rmdir;
}
