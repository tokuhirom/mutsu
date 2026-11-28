# ADR-19288's differential gate for native int kernels: a routine run as a
# kernel must answer exactly what the generic path answers, including every
# case where the kernel bails and the call is rerun generically (overflow to a
# big integer, a zero divisor, `MIN div -1`, deep recursion) and every argument
# it must refuse (Bool, IntStr, Num, Rat, a big integer), a routine wrapped after
# its kernel was built, and calls from several threads.
#
# The fixture runs three ways: kernels compiled on the first call
# (`MUTSU_JIT_THRESHOLD=1`), kernels off (`MUTSU_KERNEL=off`) and the JIT off
# (`MUTSU_JIT=off`). The shapes live in t/fixtures/kernel-int-shapes.raku.
use Test;

plan 8;

my $fixture = $?FILE.IO.parent(3).add('fixtures/kernel-int-shapes.raku').Str;
ok $fixture.IO.e, "fixture is where this test expects it ($fixture)";

sub transcript(%extra-env) {
    my %env = %*ENV;
    %env{$_} = %extra-env{$_} for %extra-env.keys;
    my $proc = run($*EXECUTABLE, $fixture, :out, :err, :%env);
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err)
}

my ($on-code, $on-out, $on-err) = transcript({ MUTSU_JIT_THRESHOLD => '1', MUTSU_VM_STATS => '1' });
my ($off-code, $off-out) = transcript({ MUTSU_KERNEL => 'off' });
my ($nojit-code, $nojit-out) = transcript({ MUTSU_JIT => 'off' });

is $on-code, 0, 'the fixture runs clean with kernels on' or diag $on-err;
is $off-code, 0, 'the fixture runs clean with kernels off';
is $nojit-code, 0, 'the fixture runs clean with the JIT off';
is $on-out, $off-out, 'kernels on and off agree on every shape';
is $on-out, $nojit-out, 'kernels and the interpreter agree on every shape';

# Not vacuous: kernels were compiled and entered, and some of them bailed.
my $stats = $on-err.lines.first(*.contains('kernel:')) // '';
ok $stats ~~ / 'compiles=' (\d+) .* 'entries=' (\d+) .* 'bails=' (\d+) / && $0 > 0 && $1 > 0 && $2 > 0,
    "kernels were compiled, entered and bailed ($stats)";

my %got = $on-out.lines.map({ .split('=', 2) }).map({ .[0] => .[1] });
is-deeply
    %got<fib tak pow2-big div mod div-zero div-min bool-arg intstr-arg depth wrap>.List,
    ('6765', '5', '1180591620717411303424', '3,-4,-4,3', '1,1,-1,-1',
     'X::Numeric::DivideByZero', '9223372036854775808', 'Bool::True', 'IntStr',
     '3000', '3000,1015').List,
    'the transcript carries the expected answers';
