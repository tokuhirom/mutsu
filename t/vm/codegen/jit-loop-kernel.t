# ADR-19288's differential gate for loop kernels: a `for` over an int range or
# a `while` run as one native loop must leave exactly what the generic loop
# leaves, including every case where an iteration bails and is rerun
# generically (overflow to a big integer, a zero divisor mid-loop), a `while`
# compiled mid-loop, NaN conditions, every slot a kernel must refuse (a typed
# variable, a subset whose check must fire at the right iteration, a Bool, a
# Str, a slot whose kind would change), the topic afterwards, `last`, and loops
# in several threads.
#
# The fixture runs three ways: kernels on, `MUTSU_KERNEL=off` and
# `MUTSU_JIT=off`. The shapes live in t/fixtures/kernel-loop-shapes.raku.
use Test;

plan 8;

my $fixture = $?FILE.IO.parent(3).add('fixtures/kernel-loop-shapes.raku').Str;
ok $fixture.IO.e, "fixture is where this test expects it ($fixture)";

sub transcript(%extra-env) {
    my %env = %*ENV;
    %env{$_} = %extra-env{$_} for %extra-env.keys;
    my $proc = run($*EXECUTABLE, $fixture, :out, :err, :%env);
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err)
}

my ($on-code, $on-out, $on-err) = transcript({ MUTSU_VM_STATS => '1' });
my ($off-code, $off-out) = transcript({ MUTSU_KERNEL => 'off' });
my ($nojit-code, $nojit-out) = transcript({ MUTSU_JIT => 'off' });

is $on-code, 0, 'the fixture runs clean with kernels on' or diag $on-err;
is $off-code, 0, 'the fixture runs clean with kernels off';
is $nojit-code, 0, 'the fixture runs clean with the JIT off';
is $on-out, $off-out, 'loop kernels on and off agree on every shape';
is $on-out, $nojit-out, 'loop kernels and the interpreter agree on every shape';

# Not vacuous: loop kernels were compiled and entered, and some bailed.
my $stats = $on-err.lines.first(*.contains('loop-kernel:')) // '';
ok $stats ~~ / 'compiles=' (\d+) .* 'entries=' (\d+) .* 'bails=' (\d+) / && $0 > 0 && $1 > 0 && $2 > 0,
    "loop kernels were compiled, entered and bailed ($stats)";

my %got = $on-out.lines.map({ .split('=', 2) }).map({ .[0] => .[1] });
is-deeply
    %got<topic decls num overflow overflow-add div-zero mandel while-long nan uint topic-after>.List,
    ('1499500', '858983', '0.3531501349087401', '1208925819614629174706176',
     '9223372036854777000', '-368,X::Numeric::DivideByZero', '19681', '8994,3000',
     '500', '0,died', 'outer,4950').List,
    'the transcript carries the expected answers';
