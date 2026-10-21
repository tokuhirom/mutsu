use Test;

# The nightly stress `precomp-verify` job failed (#14411): Test's `where`
# chunks were compiled under the lexical import chain of whichever script
# loaded Test first, so a later script with a different chain saw a cached
# compile that differed from a fresh one and stopped with status 70. A
# module's lexical scope starts empty. Both scripts share one cache, run
# in order.
plan 3;

my $cache = $*TMPDIR.add("mutsu-importer-chain-{$*PID}");
$cache.mkdir;
my $lib = $?FILE.IO.parent(3).sibling('roast').add('packages/Exportops/lib').Str;

# `use Test` is first run inside a block that imports another module.
my $first = $cache.add('first.raku');
$first.spurt: qq:to/FIRST/;
    use Test;
    plan 1;
    \{
        use lib '$lib';
        use Exportops;
        ok EVAL('5!'), 'imported postfix inside a block';
    \}
    FIRST

my $second = $cache.add('second.raku');
$second.spurt: q:to/SECOND/;
    use Test;
    plan 1;
    is-approx 1.0, 1.0, 1e-5, 'is-approx uses the where-checked tolerance';
    SECOND

my %env = %*ENV;
%env<XDG_CACHE_HOME> = $cache.add('xdg').Str;
%env<MUTSU_PRECOMP_VERIFY> = '1';
for $first, $second, $second -> $script {
    my $proc = run($*EXECUTABLE, $script.Str, :out, :err, :%env);
    $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    is $proc.exitcode, 0, "{$script.basename} verifies against the shared cache" or diag $err;
}

LEAVE { run 'rm', '-rf', $cache.Str }
