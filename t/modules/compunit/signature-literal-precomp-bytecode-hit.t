use Test;

# A module whose mainline holds a `Signature` literal used to be refused by the
# compiled-bytecode cache ("value carries an object identity: Signature"). A
# Signature that carries its SigInfo is rebuilt under a fresh id on decode, so
# it is portable: the warm run must report `hit`, and the verify mode (byte
# comparison against a fresh compile) must find no difference.
plan 3;

my $cache = $*TMPDIR.add("mutsu-sig-bytecode-{$*PID}");
my $script = $cache.add('probe.raku');
$cache.mkdir;
$script.spurt: q:to/PROBE/;
    use lib 't/lib';
    use SignatureLiteralProbe;
    say signature-probe();
    PROBE

%*ENV<XDG_CACHE_HOME> = $cache.Str;
%*ENV<MUTSU_PRECOMP_BYTECODE> = '1';
%*ENV<MUTSU_PRECOMP_VERIFY> = '1';
%*ENV<MUTSU_PRECOMP_TRACE> = '1';

sub run-probe() {
    my $proc = run($*EXECUTABLE, $script.Str, :out, :err);
    my $err = $proc.err.slurp;
    ($proc.out.slurp.chomp, $err);
}

my ($cold-out, $cold-err) = run-probe();
my ($warm-out, $warm-err) = run-probe();

like $warm-err, /'SignatureLiteralProbe.rakumod: hit'/,
    'warm run is served from the bytecode cache';
unlike $warm-err, /'differs from a fresh compile'/,
    'verify mode finds the cached compile identical to a fresh one';
is $warm-out, $cold-out, 'warm output matches cold output';

END {
    try $cache.&rmtree if $cache.e;
}
