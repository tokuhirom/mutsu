use Test;

plan 4;

# Whether constant folding is on depends on the process: once any user infix
# operator is registered, `constant` reads are no longer inlined
# (`Compiler::const_fold_enabled`). A module compiled by a cold process (nothing
# registered yet, `TAG` folded) must therefore not be served from the precomp
# cache to a process that already registered the importer's `infix:<...>`: that
# is a compile input of the cache entry (#18270). `MUTSU_PRECOMP_VERIFY` makes
# the second run compare the cached compile with a fresh one and exit 70 on a
# difference.

my $dir = $*TMPDIR.add("mutsu-const-fold-input-{$*PID}");
my $lib = $dir.add('lib');
$lib.mkdir;
$lib.add('FoldLeaf.rakumod').spurt: q:to/LEAF/;
    unit module FoldLeaf;
    constant TAG = 'one';
    sub leaf-tag() is export { TAG }
    LEAF
$lib.add('FoldTop.rakumod').spurt: q:to/TOP/;
    unit module FoldTop;
    use FoldLeaf;
    sub infix:<topadd>($a, $b) is export { $a + $b }
    sub top-leaf-tag() is export { leaf-tag() }
    TOP
my $script = $dir.add('prog.raku');
$script.spurt: "use FoldTop; say 1 topadd 2; say top-leaf-tag();\n";

%*ENV<XDG_CACHE_HOME> = $dir.add('cache').Str;
%*ENV<MUTSU_PRECOMP_VERIFY> = '1';

sub run-script() {
    my $proc = run($*EXECUTABLE, '-I', $lib.Str, $script.Str, :out, :err);
    %( out => $proc.out.slurp, err => $proc.err.slurp, exit => $proc.exitcode )
}

my %cold = run-script();
is %cold<exit>, 0, 'cold run succeeds';
is %cold<out>, "3\none\n", 'cold run output';

my %warm = run-script();
is %warm<exit>, 0, 'warm run: the cached compile matches a fresh one';
is %warm<out>, "3\none\n", 'warm run output';

END { run 'rm', '-rf', $dir.Str if $dir.e }
