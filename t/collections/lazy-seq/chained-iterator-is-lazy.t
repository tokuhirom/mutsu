use Test;

# A built-in iterator reached through a call result keeps its own `is-lazy`
# when the program composes the Iterator role (#16934).

plan 6;

class It does Iterator { method pull-one { IterationEnd } }

is [4, 5, 6].lazy.iterator.is-lazy, True, 'lazy array iterator, chained';
is (1..*).iterator.is-lazy, True, 'infinite range iterator, chained';
my $i = [4, 5, 6].lazy.iterator;
is $i.is-lazy, True, 'the same through a variable';
is (1..3).iterator.is-lazy, False, 'finite range iterator';
is [1, 2].iterator.is-lazy, False, 'eager array iterator';
is (1, 2, 3).iterator.pull-one, 1, 'a chained pull-one still advances';
