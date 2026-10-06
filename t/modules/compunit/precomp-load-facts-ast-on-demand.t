use Test;

# ADR-12026 §2.1: a precompilation hit loads a module from the facts recorded
# with its compiled section and leaves the AST encoded, unless something the
# facts cannot serve asks for it. This module asks three ways at once
# (declarator docs, a `state` sub, a block phaser), so the warm run must
# decode the entry's own AST and still agree with the cold run.
# MUTSU_PRECOMP_VERIFY=1 also recomputes the load facts from that AST and
# stops with status 70 on any difference.
plan 5;

my $lib = $?FILE.IO.parent(2).sibling('lib').Str;
my $cache = $*TMPDIR.add("mutsu-load-facts-{$*PID}");
my $script = $cache.add('probe.raku');

$cache.mkdir;
$script.spurt: qq:to/PROBE/;
    use lib '$lib';
    use PrecompLoadFactsProbe;
    say probe();
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

# `not yet`: a unit's ENTER phaser has not run when `probe` reads `$left`
# (rakudo prints the same).
my $expected = '2|10|20|not yet';

my ($cold-code, $cold-out, $cold-err) = run-probe();
is $cold-out, $expected, 'cold run: the module answers correctly'
    or diag $cold-err;

my ($warm-code, $warm-out, $warm-err) = run-probe();
is $warm-code, 0, 'warm run: recorded facts and cached compile verify'
    or diag $warm-err;
like $warm-err, /'PrecompLoadFactsProbe.rakumod: hit'/,
    'warm run: the mainline is served from the cache';
is $warm-out, $expected, 'warm run: the same answers, AST decoded on demand';
is $cold-code, 0, 'cold run exits cleanly';

END {
    try $cache.&rmtree if $cache.e;
}

sub rmtree(IO::Path $d) {
    for $d.dir -> $e { $e.d ?? rmtree($e) !! $e.unlink }
    $d.rmdir;
}
