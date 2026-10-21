use Test;

# A module's top-level `use` binds the variables it imports in the module's
# pad, as the parse-time scan of the used module lists them (ADR-8751, #14140).
# The module's compiled mainline is cached (ADR-11756), so that scan is one of
# its compile's inputs: when the used module changes how it exports a name,
# the importer's cached bytecode must not be served. Pinned end to end, across
# separate processes, because the cache only exists on disk.

plan 4;

my $dir = $*TMPDIR.add("mutsu-use-scan-revalidated-{$*PID}");
my $lib = $dir.add('lib');
$lib.mkdir;

# A cache root of its own, so the first run really is cold.
%*ENV<XDG_CACHE_HOME> = $dir.add('cache').Str;

my $leaf = $lib.add('UseScanLeaf.rakumod');
$lib.add('UseScanMid.rakumod').spurt: q:to/MID/;
    unit module UseScanMid;
    use UseScanLeaf;
    sub mid-reads() is export { $use-scan-v }
    MID
my $script = $dir.add('prog.raku');
$script.spurt: q:to/PROG/;
    my $use-scan-v = 'caller';
    use UseScanMid;
    say mid-reads();
    PROG

sub run-script() {
    my $proc = run($*EXECUTABLE, '-I', $lib.Str, $script.Str, :out, :err);
    my $out = $proc.out.slurp;
    my $err = $proc.err.slurp;
    %( :$out, :$err, exit => $proc.exitcode )
}

$leaf.spurt: q:to/LEAF/;
    unit module UseScanLeaf;
    our $use-scan-v is export = 'trait';
    LEAF
is run-script()<out>, "trait\n", 'cold run reads the exported variable';
is run-script()<out>, "trait\n", 'warm run serves the same';

# The same name, now handed out by a `sub EXPORT` hook: the importer's pad
# binding reads it from the hook's export set instead of the package.
$leaf.spurt: q:to/LEAF/;
    sub EXPORT { %( '$use-scan-v' => 'hooked' ) }
    LEAF
is run-script()<out>, "hooked\n", 'a changed export of the used module recompiles the importer';
is run-script()<out>, "hooked\n", 'and the recompiled importer is served warm';

END {
    try rmtree($dir) if $dir.e;
}

sub rmtree(IO::Path $d) {
    for $d.dir -> $e {
        $e.d ?? rmtree($e) !! $e.unlink;
    }
    $d.rmdir;
}
