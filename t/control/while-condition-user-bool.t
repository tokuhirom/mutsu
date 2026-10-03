use Test;

plan 4;

# A loop condition is a boolean context like `if`: a user `Bool` method
# decides it.
class Stack {
    has @.items;
    method Bool { ?@!items }
    method drain-while { my @r; @r.push(@!items.pop) while self; @r }
    method drain-gather { gather take @!items.pop while self }
    method drain-repeat { my @r; repeat { @r.push(@!items.pop) } while self; @r }
}

is-deeply Stack.new(items => [1, 2]).drain-while, [2, 1], '`while self` calls the user Bool';
is-deeply Stack.new(items => [1, 2]).drain-gather.List, (2, 1), '`gather take ... while self` stops';
is-deeply Stack.new(items => [1, 2, 3]).drain-repeat, [3, 2, 1], '`repeat ... while self` stops';

my $n = 0;
loop (my $i = 0; Stack.new(items => $n < 2 ?? [1] !! []); $i++) { $n++ }
is $n, 2, 'C-style loop condition calls the user Bool';
