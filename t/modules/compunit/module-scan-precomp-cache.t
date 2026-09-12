use Test;

# The parser scans a `use`d module's source to learn what it exports, and since
# GH-8095 that scan is cached on disk (`src/scan_cache.rs`) the way the run-time
# AST already was. A scan hit performs no parse at all, so whatever the scan
# used to register as a side effect must now be replayed from the entry -- and
# whatever the entry depends on must invalidate it.
#
# Both halves are pinned here end to end, across separate processes, because
# that is the only place the disk cache exists: within one process the scan is
# memoized in a thread-local and the bug class cannot appear.
#
# An *exported operator* is the probe throughout: `1 topadd 2` only parses when
# the parser already knows `infix:<topadd>` was imported, so a lost or stale
# export list is a hard compile failure rather than a subtle difference.

plan 6;

my $dir = $*TMPDIR.add("mutsu-scan-cache-{$*PID}");
my $lib = $dir.add('lib');
$lib.mkdir;

# Give the run its own cache root so the first run really is cold, whatever the
# developer's ~/.cache happens to hold.
%*ENV<XDG_CACHE_HOME> = $dir.add('cache').Str;

my $leaf = $lib.add('ScanCacheLeaf.rakumod');
my $top = $lib.add('ScanCacheTop.rakumod');
my $script = $dir.add('prog.raku');

sub write-leaf($tag) {
    $leaf.spurt: qq:to/LEAF/;
        unit module ScanCacheLeaf;
        constant SCAN_CACHE_LEAF_TAG is export = '$tag';
        sub leaf-tag() is export \{ SCAN_CACHE_LEAF_TAG }
        LEAF
}

sub write-top($op) {
    $top.spurt: qq:to/TOP/;
        unit module ScanCacheTop;
        use ScanCacheLeaf;
        class ScanCacheTop::Marker is export \{ }
        sub infix:<$op>(\$a, \$b) is export \{ \$a + \$b }
        sub top-leaf-tag() is export \{ leaf-tag() }
        TOP
}

sub write-script($op) {
    $script.spurt: qq:to/PROG/;
        use ScanCacheTop;
        say 1 $op 2;
        say top-leaf-tag();
        say ScanCacheTop::Marker.new.defined;
        PROG
}

sub run-script() {
    my $proc = run($*EXECUTABLE, '-I', $lib.Str, $script.Str, :out, :err);
    my $out = $proc.out.slurp;
    my $err = $proc.err.slurp;
    %( :$out, :$err, exit => $proc.exitcode )
}

write-leaf('one');
write-top('topadd');
write-script('topadd');

my %cold = run-script();
is %cold<exit>, 0, 'cold run compiles and runs';
is %cold<out>, "3\none\nTrue\n", 'cold run sees the imported operator and sub';

my %warm = run-script();
is %warm<out>, %cold<out>,
    'a warm module-scan cache replays the export registrations identically';

# The module's own source changing must invalidate its entry. A stale entry
# would still list `topadd` and know nothing of `topmul`, so the new program
# would fail to parse rather than quietly compute the old answer.
write-top('topmul');
write-script('topmul');
my %edited = run-script();
is %edited<exit>, 0, 'an edited module is re-scanned, not served from the entry';
is %edited<out>, "3\none\nTrue\n", 'the renamed operator is imported';

# A scan result carries names that reached the module from its *dependencies*,
# so editing one must re-validate the importer's entry too (the precise
# invalidation rules are unit-tested in `src/scan_cache.rs`; what matters here
# is that a dependency edit still produces a correct, consistent run).
write-leaf('two');
my %dep-edited = run-script();
is %dep-edited<out>, "3\ntwo\nTrue\n",
    'an edited dependency is picked up through the importer';

END {
    # `run` may still hold the directory; ignore a failed cleanup.
    try rmtree($dir) if $dir.e;
}

sub rmtree(IO::Path $d) {
    for $d.dir -> $e {
        $e.d ?? rmtree($e) !! $e.unlink;
    }
    $d.rmdir;
}
