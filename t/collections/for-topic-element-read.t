use Test;

# A `for` body that reads array elements (`@a[$i]`) still cannot observe how
# its parameter or topic is bound, so the loop binds only the slot (#19406).
# Every way the binding can be seen must behave as before: the value, the
# enclosing variable of the same name, the readonly parameter, a closure, a
# write to the element, and a read from another thread.

plan 10;

my @a = ^256;

sub sum-param() {
    my $s = 0;
    for ^1000 -> $i { $s += @a[$i +& 255] }
    $s
}
is sum-param(), 124716, 'a named parameter subscripts an array';

sub sum-topic() {
    my $s = 0;
    for ^1000 { $s += @a[$_ % 256] }
    $s
}
is sum-topic(), 124716, 'the topic subscripts an array';

{
    my $i = 'outer';
    my $s = 0;
    for ^10 -> $i { $s += @a[$i] }
    is $s, 45, 'loop sum with a same-named outer variable';
    is $i, 'outer', 'the outer variable of the same name is untouched';
}

{
    $_ = 'outer';
    my $s = 0;
    for ^10 { $s += @a[$_] }
    is $_, 'outer', 'the enclosing topic is back after the loop';
}

dies-ok { for ^3 -> $i { my $x = @a[$i]; $i = 5 } },
    'the parameter is still readonly';

{
    my @b = 0 xx 4;
    for ^4 -> $i { @b[$i] = @a[$i] * 2 }
    is @b.join(','), '0,2,4,6', 'an element write in the same body';
}

{
    my @subs;
    for ^3 -> $i { my $v = @a[$i]; @subs.push: { $i + $v } }
    is @subs.map({ $_() }).join(','), '0,2,4', 'a closure still captures each iteration';
}

{
    my @alias := @a;
    my $p = start {
        my $s = 0;
        for ^1000 -> $i { $s += @alias[$i +& 255] }
        $s
    };
    is await($p), 124716, 'a captured array read from a start block';
}

{
    my @nested = [1, 2], [3, 4];
    my $s = 0;
    for ^2 -> $i { $s += @nested[$i][1] }
    is $s, 6, 'a nested element read';
}
