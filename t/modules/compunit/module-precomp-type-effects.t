use Test;

# A cached module must replay the parser's type index before its AST is
# compiled. Lowercase native aliases in return constraints are otherwise
# mistaken for an already-specified return value on warm runs.
plan 4;

my $dir = $*TMPDIR.add("mutsu-precomp-type-effects-{$*PID}");
my $lib = $dir.add('lib');
$lib.mkdir;
%*ENV<XDG_CACHE_HOME> = $dir.add('cache').Str;
%*ENV<MUTSU_PRECOMP> = '1';

$lib.add('PrecompTypeEffects.rakumod').spurt: q:to/MODULE/;
    unit module PrecompTypeEffects;
    constant time = int64;
    sub typed(--> time) {
        return -1;
    }
    sub answer is export { typed() }
    MODULE

my $script = $dir.add('program.raku');
$script.spurt: "use PrecompTypeEffects; say answer();\n";

sub run-program() {
    my $proc = run($*EXECUTABLE, '-I', $lib.Str, $script.Str, :out, :err);
    my $out = $proc.out.slurp;
    my $err = $proc.err.slurp;
    %( :$out, :$err, exit => $proc.exitcode )
}

my %cold = run-program();
is %cold<exit>, 0, 'cold run compiles the type-alias module';
is %cold<out>, "-1\n", 'cold run returns through the lowercase alias';

my %warm = run-program();
is %warm<exit>, 0, 'warm run compiles the cached type-alias AST';
is %warm<out>, %cold<out>, 'warm run preserves the type-alias result';

END {
    try rmtree($dir) if $dir.e;
}

sub rmtree(IO::Path $d) {
    for $d.dir -> $e {
        $e.d ?? rmtree($e) !! $e.unlink;
    }
    $d.rmdir;
}
