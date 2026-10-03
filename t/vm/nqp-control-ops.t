use Test;
use nqp;

# The definedness-testing nqp:: control forms (#11500): their branches are
# thunks, evaluated only when selected, and the test is `.defined`.

plan 20;

# nqp::with / nqp::without pick a branch by definedness, not truthiness.
is nqp::with(42, 'a', 'b'), 'a', 'with: defined condition takes the then-branch';
is nqp::with(Any, 'a', 'b'), 'b', 'with: undefined condition takes the else-branch';
is nqp::with(0, 'd', 'u'), 'd', 'with: 0 is defined';
is nqp::without(Any, 'a', 'b'), 'a', 'without: undefined condition takes the then-branch';
is nqp::without('', 'a', 'b'), 'b', 'without: the empty string is defined';

# A missing else-branch yields the condition itself.
ok nqp::with(Int, 1) === Int, 'with: no else yields the undefined condition';
is nqp::without(42, 1), 42, 'without: no else yields the defined condition';

# A Failure is undefined.
is nqp::with(Failure.new, 1, 2), 2, 'with: a Failure takes the else-branch';

# Only the selected branch is evaluated.
{
    my @log;
    nqp::with(42, @log.push('then'), @log.push('else'));
    nqp::with(Any, @log.push('then'), @log.push('else'));
    nqp::without(Any, @log.push('then'), @log.push('else'));
    nqp::without(5, @log.push('then'), @log.push('else'));
    is-deeply @log, ['then', 'else', 'then', 'else'], 'with/without evaluate one branch';
}

# The condition is evaluated exactly once.
{
    my $n = 0;
    is nqp::with(++$n, 'x', 'y'), 'x', 'with: side-effecting condition';
    is $n, 1, 'with: condition evaluated once';
}

# A block operand is a value, not called.
ok nqp::with(42, -> $x { $x + 1 }) ~~ Block, 'with: a block branch is yielded, not called';

# Variables work as conditions.
{
    my $x = Any;
    my $y = 3;
    is nqp::with($x, 1, 2), 2, 'with: undefined variable';
    is nqp::without($y, 1), 3, 'without: defined variable yields itself';
}

# nqp::defor is `//`: the fallback runs only when needed.
is nqp::defor(Any, 7), 7, 'defor: undefined value takes the fallback';
is nqp::defor(0, 9), 0, 'defor: 0 is defined';
is nqp::defor(5, die('not evaluated')), 5, 'defor: fallback is not evaluated';
{
    my $c = 0;
    my @r = nqp::defor(Any, $c++), nqp::with(1, $c++, $c++);
    is $c, 2, 'defor/with evaluate only the selected operands';
}

# Inside a sub, where the TRIR tier may compile the body.
{
    sub pick($v) { nqp::defor($v, 'dflt') }
    is pick(Str), 'dflt', 'defor in a sub: undefined';
    is pick('v'), 'v', 'defor in a sub: defined';
}
