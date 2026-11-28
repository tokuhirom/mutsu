# A `for` over a materialized list of plain Ints, whose body reads `$_`
# through its own slot, runs as a loop kernel (vm_for_loop_list_kernel). It
# must leave exactly what the generic loop leaves, including a bail that
# reruns an iteration generically and lists the kernel must refuse.
#
# The fixture runs three ways: kernels on, `MUTSU_KERNEL=off` and
# `MUTSU_JIT=off`. The shapes live in t/fixtures/kernel-list-shapes.raku.
use Test;

plan 7;

my $fixture = $?FILE.IO.parent(2).add('fixtures/kernel-list-shapes.raku').Str;
ok $fixture.IO.e, "fixture is where this test expects it ($fixture)";

sub transcript(%extra-env) {
    my %env = %*ENV;
    %env{$_} = %extra-env{$_} for %extra-env.keys;
    my $proc = run($*EXECUTABLE, $fixture, :out, :err, :%env);
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err)
}

my ($on-code, $on-out, $on-err) = transcript({});
my ($off-code, $off-out) = transcript({ MUTSU_KERNEL => 'off' });
my ($nojit-code, $nojit-out) = transcript({ MUTSU_JIT => 'off' });

is $on-code, 0, 'the fixture runs clean with kernels on' or diag $on-err;
is $off-code, 0, 'the fixture runs clean with kernels off';
is $nojit-code, 0, 'the fixture runs clean with the JIT off';
is $on-out, $off-out, 'list kernels on and off agree on every shape';
is $on-out, $nojit-out, 'list kernels and the interpreter agree on every shape';

my %got = $on-out.lines.map({ .split('=', 2) }).map({ .[0] => .[1] });
is-deeply
    %got<values list overflow mixed-kinds num-acc topic-after short div-zero>.List,
    ('999000', '41791250', '1208925819614629174706176', '41', '2525',
     'outer,1275', '6,0', '389,X::Numeric::DivideByZero').List,
    'the transcript carries the expected answers';
