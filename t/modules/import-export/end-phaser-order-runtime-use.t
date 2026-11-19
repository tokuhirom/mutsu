use Test;

# ENDs run in reverse install order, and rakudo installs every `use` at
# compile time, in source order. mutsu loads `use Test` at run time while the
# parse loads an ordinary module ahead of it, so a module `use`d after Test
# used to run its END after Test's instead of before it (#18183). Green's END,
# which waits for its worker's tests, then ran after Test's plan check.

plan 3;

# The lines a program prints from its ENDs and Test's plan check, in order.
sub end-lines(Str $code) {
    my $p = run $*EXECUTABLE, '-I', 't/lib', '-e', $code, :out, :merge;
    $p.out.slurp(:close).lines.grep(/'END' | 'planned'/).join('|')
}

is end-lines(q:to/CODE/), 'EndOrderLate END|# You planned 2 tests, but ran 1',
    use Test;
    use EndOrderLate;
    plan 2;
    ok 1;
    CODE
    "a module used after Test runs its END before Test's";

is end-lines(q:to/CODE/), '# You planned 2 tests, but ran 1|EndOrderLate END',
    use EndOrderLate;
    use Test;
    plan 2;
    ok 1;
    CODE
    "a module used before Test runs its END after Test's";

is end-lines(q:to/CODE/), 'main END|EndOrderLate END|# You planned 2 tests, but ran 1',
    use Test;
    use EndOrderLate;
    plan 2;
    ok 1;
    END { say "main END" }
    CODE
    "the main unit's END runs first";
