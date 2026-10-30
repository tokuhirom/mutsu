use Test;

# The `IterationEnd` sentinel is a CORE term: a bareword read finds it without
# consulting GLOBAL, from a class method as from the mainline, and it is the one
# object `pull-one` returns when the iterator is exhausted.
plan 7;

class Counter does Iterator {
    has Int $.n = 0;
    method pull-one { $!n < 2 ?? $!n++ !! IterationEnd }
    method sentinel { IterationEnd }
}

my $it = Counter.new;
is $it.pull-one, 0, 'first value';
is $it.pull-one, 1, 'second value';
ok $it.pull-one =:= IterationEnd, 'an exhausted iterator returns the sentinel';
ok $it.sentinel =:= IterationEnd, 'a method reads the same sentinel';
is IterationEnd.^name, 'Mu', 'the sentinel is a plain Mu instance';

module Outer::Inner {
    our sub sentinel { IterationEnd }
}
ok Outer::Inner::sentinel() =:= IterationEnd, 'a sub in a nested package reads it too';

# A lexical declaration of the same name still wins over the CORE term.
{
    my constant IterationEnd = 42;
    is IterationEnd, 42, 'a constant declared in scope shadows the core sentinel';
}
