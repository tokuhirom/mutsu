use Test;

# One directory named relatively by `use lib` and absolutely by `-I` is one
# repository: rakudo makes a `use lib` path absolute against `$*CWD`. mutsu
# kept the relative spelling, so a parse-time probe (which searches the
# command-line chain) and the unit itself loaded the module file under two
# paths, and the two loads overwrote each other's precompiled entry on every
# run (#17690). The module's duplicated-trait warning names the file it was
# parsed from, so one warning, naming the absolute path, means one load.

plan 3;

my $cache = $*TMPDIR.add("mutsu-use-lib-abs-{$*PID}");
$cache.mkdir;
my $script = $cache.add('probe.raku');
$script.spurt: q:to/PROBE/;
    use lib 't/lib';
    use PrecompRevisionProbe;
    say precomp-probe-hello();
    say $*REPO.path-spec;
    PROBE
%*ENV<XDG_CACHE_HOME> = $cache.Str;

my $abs = $*CWD.add('t/lib').Str;
sub run-probe() {
    my $proc = run($*EXECUTABLE, '-I', $abs, $script.Str, :out, :err);
    %( out => $proc.out.slurp, err => $proc.err.slurp )
}

my %cold = run-probe();
is %cold<err>.comb('Duplicate').elems, 1, 'the module is parsed once on a cold run';
ok %cold<err>.contains($abs), 'under its absolute path';
ok %cold<out>.contains("file#$abs"), 'the use-lib repository spec is absolute';

END {
    if $cache.e {
        run 'rm', '-rf', $cache.Str;
    }
}
