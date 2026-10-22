use Test;

# The precompilation cache holds bytecode a later load executes (ADR-11756), so
# nobody but the current user may write it (docs/security.md, "Caches are keyed
# and private"; #14516). Every directory mutsu creates under the cache root is
# 0700, every entry 0600, and a directory an older build left behind with a
# looser mode is tightened.

plan 6;

my $root = $*TMPDIR.add("mutsu-precomp-private-{$*PID}");
my $lib = $root.add('lib');
my $cache = $root.add('cache');
$lib.mkdir;
$cache.mkdir;
$lib.add('PrivateCacheProbe.rakumod').spurt: q:to/MOD/;
    unit module PrivateCacheProbe;
    sub private-cache-probe() is export { 42 }
    MOD
my $script = $root.add('probe.raku');
$script.spurt: qq:to/PROBE/;
    use lib '{$lib}';
    use PrivateCacheProbe;
    say private-cache-probe();
    PROBE

# A cache directory created by an older build with the umask's mode.
my $mutsu-dir = $cache.add('mutsu');
$mutsu-dir.mkdir;
$mutsu-dir.chmod(0o777);

%*ENV<XDG_CACHE_HOME> = $cache.Str;

sub run-probe() {
    my $proc = run($*EXECUTABLE, $script.Str, :out, :err);
    $proc.out.slurp(:close).trim
}

is run-probe(), '42', 'cold run loads the module';
is run-probe(), '42', 'warm run loads the module from the cache';

sub mode-of(IO::Path $p) { $p.mode.Int +& 0o777 }

is mode-of($mutsu-dir), 0o700, 'a looser cache directory of ours is tightened to 0700';
my $precomp = $mutsu-dir.add('precomp');
is mode-of($precomp), 0o700, 'the precomp directory is created 0700';
my @entries = $precomp.dir.grep(*.extension eq any <bin code>);
ok @entries > 0, 'the cold run wrote cache entries';
is @entries.map(&mode-of).unique.list, (0o600,), 'every cache entry is 0600';

END {
    sub rm-tree(IO::Path $p) {
        if $p.d && !$p.l { rm-tree($_) for $p.dir; $p.rmdir }
        else { $p.unlink }
    }
    rm-tree($root) if $root.e;
}
