use Test;

# A `for ... -> $h` parameter binds a fresh `$h` for each iteration, whatever
# else the unit declares as `$h` later on: a store through the loop variable
# reaches every element, not only the first.

plan 7;

{
    my $a = [ {a => 1}, {a => 2} ];
    for $a.list -> $h { $h<x> = 1 }
    for 1, 2 -> $j, $h { }
    is $a.raku, '$[{:a(1), :x(1)}, {:a(2), :x(1)}]', 'a later two-parameter loop naming $h';
}
{
    my $a = [ {a => 1}, {a => 2}, {a => 3} ];
    for $a.list -> $h { $h<x> = 1 }
    my $h;
    is $a.raku, '$[{:a(1), :x(1)}, {:a(2), :x(1)}, {:a(3), :x(1)}]', 'a later my $h';
    nok $h.defined, 'and the later $h is untouched';
}
{
    my $a = [ {a => 1}, {a => 2} ];
    for $a.list -> $h { $h<x> = 1 }
    for 1, 2 -> $h, $j { }
    is $a.raku, '$[{:a(1), :x(1)}, {:a(2), :x(1)}]', 'a later loop with $h first';
}
{
    my $a = [ {a => 1}, {a => 2} ];
    for $a.list -> $h { $h<x> = 1 }
    sub f($j, $h) { }
    is $a.raku, '$[{:a(1), :x(1)}, {:a(2), :x(1)}]', 'a later sub parameter';
}
{
    my @seen;
    for 1, 2, 3 -> $n { @seen.push($n * 2) }
    for 1, 2 -> $k, $n { }
    is @seen.join(','), '2,4,6', 'scalar loop values are unaffected';
}
{
    my $h = 'outer';
    for 1, 2 -> $h { }
    is $h, 'outer', 'an enclosing $h is restored after the loop';
}
