# ADR-0116: TRIR chunks lowered to native code must answer exactly what the
# TRIR switch loop and the untyped path answer.
#
# Every `t/fixtures/trir-*.raku` fixture (the shapes the other TRIR pins
# cover, plus `trir-native.raku` for the ops the lowering emits inline) runs
# three ways: native code from a chunk's first run (MUTSU_JIT_THRESHOLD=1),
# the lowering off (MUTSU_TRIR_JIT=off), and TRIR off (MUTSU_TRIR=off). The
# transcripts and exit codes must be identical, and the native runs must
# actually have compiled chunks, so the agreement is not vacuous.
use Test;

my $dir = $?FILE.IO.parent(3).add('fixtures');
my @fixtures = $dir.dir.grep({ .basename ~~ /^ 'trir-' .* '.raku' $/ }).sort;

plan 2 * @fixtures + 3;

sub transcript($file, %extra-env) {
    my %env = %*ENV;
    %env{$_} = %extra-env{$_} for %extra-env.keys;
    my $proc = run($*EXECUTABLE, '-I', $dir.Str, $file.Str, :out, :err, :%env);
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err)
}

my $compiled = 0;
my $native-runs = 0;
for @fixtures -> $file {
    my ($n-code, $n-out, $n-err) = transcript($file,
        { MUTSU_JIT => 'on', MUTSU_JIT_THRESHOLD => '1', MUTSU_VM_STATS => '1' });
    my ($i-code, $i-out, $i-err) = transcript($file,
        { MUTSU_JIT => 'on', MUTSU_TRIR_JIT => 'off' });
    my ($u-code, $u-out, $u-err) = transcript($file, { MUTSU_TRIR => 'off' });
    is "$n-code\n$n-out", "$i-code\n$i-out",
        "{$file.basename}: native TRIR == the TRIR switch loop"
        or diag "native stderr:\n$n-err";
    is "$n-code\n$n-out", "$u-code\n$u-out",
        "{$file.basename}: native TRIR == the untyped path";
    if $n-err ~~ /'native: compiled=' (\d+) ' declined=' \d+ ' runs=' (\d+)/ {
        $compiled += +$0;
        $native-runs += +$1;
    }
}
ok $compiled > 0, "the fixtures compiled chunks to native code ($compiled)";
ok $native-runs > 0, "and ran them ($native-runs runs)";

# The inline ops' own transcript, checked against rakudo.
my ($code, $out, $err) = transcript($dir.add('trir-native.raku'),
    { MUTSU_JIT => 'on', MUTSU_JIT_THRESHOLD => '1' });
is $out, q:to/END/, 'the inline ops answer what rakudo answers';
    int-ops=-308645 250
    cmp-ops=35 26 44
    wraps=-2 -9223372036854775805
    num-ops=2.671875 0.9375
    num-cmp=1 2 4
    sized=4400300 25499999 6374464
    tri=55 500500
    short=5 3 43
    bump=7 7
    bump-twice=11 11
    caller-slot=9020
    skip-ws=4 4
    divide=3 -4
    divide-by-zero=died
    int-ops=-308645 250
    cmp-ops=35 26 44
    wraps=-2 -9223372036854775805
    num-ops=2.671875 0.9375
    num-cmp=1 2 4
    sized=4400300 25499999 6374464
    tri=55 500500
    short=5 3 43
    bump=7 7
    bump-twice=11 11
    caller-slot=9020
    skip-ws=4 4
    divide=3 -4
    divide-by-zero=died
    raku-mod=3 -3 2
    nqp-mod=-2 2 2
    raku-mod=3 -3 2
    nqp-mod=-2 2 2
    END
