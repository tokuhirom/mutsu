use Test;

# A `for ... -> $w` loop whose body cannot observe how `$w` is bound reads it
# only through its own slot: the VM skips the per-item env binding and
# readonly mark. Everything the parameter's binding can be seen through must
# behave as before.

plan 11;

# The word-count shape.
{
    my %c;
    for <a b a c b a> -> $w { %c{$w}++ }
    is %c.sort.map({ .key ~ '=' ~ .value }).join(','), 'a=3,b=2,c=1', 'counting into a hash';
}

# An outer variable of the same name is untouched and visible after the loop.
{
    my $w = 'outer';
    my $sum = 0;
    for 1..4 -> $w { $sum += $w }
    is $sum, 10, 'the parameter is read in the body';
    is $w, 'outer', 'the outer variable is back after the loop';
}

# Assigning to the parameter still dies (that body is not slot-only).
dies-ok { for 1..2 -> $w { $w = 5 } }, 'the parameter is readonly';

# A closure capturing the parameter sees each iteration's value.
{
    my @subs;
    for 1..3 -> $w { @subs.push({ $w * 10 }) }
    is @subs.map({ .() }).join(','), '10,20,30', 'closures capture per-iteration values';
}

# Nested loops with different parameters.
{
    my $acc = '';
    for 1..2 -> $i { for <x y> -> $j { $acc ~= "$i$j " } }
    is $acc, '1x 1y 2x 2y ', 'nested named-parameter loops';
}

# next/last, statement-modifier if.
{
    my $acc = 0;
    for 1..10 -> $n { next if $n %% 2; last if $n > 7; $acc += $n }
    is $acc, 16, 'next and last';
}

# Decrementing a hash element.
{
    my %c = a => 5;
    for <a a> -> $k { %c{$k}-- }
    is %c<a>, 3, 'hash element decrement';
}

# A pre-increment of a hash element.
{
    my %c;
    for <z z z> -> $k { ++%c{$k} }
    is %c<z>, 3, 'hash element pre-increment';
}

# Over an int range (the range driver).
{
    my %c;
    for ^5 -> $i { %c{$i % 2}++ }
    is %c<0>, 3, 'counting over an int range';
}

# The parameter read in a string interpolation.
{
    my $s = '';
    for <p q> -> $w { $s ~= "<$w>" }
    is $s, '<p><q>', 'interpolated parameter';
}
