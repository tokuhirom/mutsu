use Test;

# A `for` loop whose body cannot observe its topic's container reads `$_`
# from a slot of its own, and the env's `_` is not rebound per item. Every
# way the enclosing topic, the loop topic or a source variable can be seen
# must still behave as before.

plan 16;

# The int-range loop of benchmarks/hash-access.raku.
sub fill() {
    my %h;
    for ^5 { %h{"key-$_"} = $_ * 2 }
    %h
}
is fill().sort.map({ .key ~ '=' ~ .value }).join(','),
    'key-0=0,key-1=2,key-2=4,key-3=6,key-4=8', 'Str-keyed store from an int range';

# The enclosing topic is untouched, during and after the loop.
{
    $_ = 'outer';
    my @seen;
    for 1..3 { my $x = $_ * 10; @seen[0] = 1 if $x > 100 }
    is $_, 'outer', 'enclosing topic is back after an int-range loop';
    my $sum = 0;
    for <1 2 3> { $sum += $_ }
    is $sum, 6, 'list loop sums its topic';
    is $_, 'outer', 'enclosing topic is back after a list loop';
}

# A sub's own `$_` parameter keeps the existing mirrored slot.
sub topic-param($_) {
    my $s = 0;
    for 1..3 { $s += $_ }
    "$_:$s"
}
is topic-param('p'), 'p:6', 'a $_ parameter is restored after the loop';

# `.values` loop over a hash.
{
    my %h = a => 1, b => 2, c => 3;
    my $sum = 0;
    for %h.values { $sum += $_ }
    is $sum, 6, '.values loop';
}

# A body with `next`/`last` and statement-modifier `if`.
{
    my $acc = '';
    for ^10 { next if $_ %% 2; last if $_ > 7; $acc ~= $_ }
    is $acc, '1357', 'next/last in a slot-topic body';
}

# A nested bare block reads the loop topic.
{
    my $acc = '';
    for 1..3 { { $acc ~= $_ } }
    is $acc, '123', 'nested bare block sees the loop topic';
}

# Loops nested in one another: the inner one owns its topic.
{
    my $acc = '';
    for 1..2 { my $o = $_; for 1..2 { $acc ~= "$o$_ " }; $acc ~= "($_) " }
    is $acc, '11 12 (1) 21 22 (2) ', 'nested loops each see their own topic';
}

# A source variable assigned inside the body keeps the assignment.
{
    my $a = 1;
    for $a { $a = 5 }
    is $a, 5, 'a scalar source assigned in the body keeps the new value';
}

# The statement-modifier form.
{
    my $n = 0;
    $n += $_ for 1..4;
    is $n, 10, 'statement-modifier for';
}

# Collected values of a `do for`.
{
    my @r = do for 1..3 { $_ * 2 };
    is @r.join(','), '2,4,6', 'do for collects the topic-derived values';
}

# A loop after a slot-topic loop reads the enclosing topic by name again.
{
    $_ = 'x';
    for 1..2 { my $y = $_ }
    is "$_", 'x', '$_ after the loop is the enclosing topic';
}

# A gather/take around the loop is not unobservable (take is a call), but
# a plain loop inside a gather still works.
{
    my @g = gather { my $s = 0; for 1..3 { $s += $_ }; take $s };
    is @g.join(','), '6', 'slot-topic loop inside gather';
}

# A loop in a closure called repeatedly.
{
    my &f = -> $n { my $t = 0; for ^$n { $t += $_ }; $t };
    is (f(3), f(4)).join(','), '3,6', 'slot-topic loop in a closure';
}

# A loop over a lazy range.
{
    my $c = 0;
    for 1..* { last if $_ > 5; $c += $_ }
    is $c, 15, 'unbounded range';
}
