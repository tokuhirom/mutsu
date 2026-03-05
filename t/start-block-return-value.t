use Test;

plan 5;

# If/else as last statement in start block should return the branch value
{
    my $p = start { if True { 99 } else { 0 } };
    is await($p), 99, 'if/else as last statement in start block returns branch value';
}

# Ternary in start block should also work (baseline)
{
    my $p = start { True ?? 42 !! 0 };
    is await($p), 42, 'ternary in start block returns correct value';
}

# Recursive start blocks with await should not corrupt $n
{
    sub conc-fib($n) {
        start {
            $n <= 1
                ?? 1
                !! await(conc-fib($n - 2)) + await(conc-fib($n - 1))
        }
    }
    is await(conc-fib(5)), 8, 'recursive start/await fibonacci computes correctly';
}

# Start blocks should isolate function parameters
{
    sub foo($n) {
        start { $n <= 0 ?? 99 !! await(foo($n - 1)) }
    }
    is await(foo(2)), 99, 'recursive start with base case works';
}

# Atomic operations still work across threads
{
    my atomicint $i = 0;
    await start { for ^100 { $i⚛++; } } xx 4;
    is atomic-fetch($i), 400, 'atomic increment works across start/await';
}
