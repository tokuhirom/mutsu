use v6;
use Test;

# A sigilless variable in a positional binding is a writable alias when its
# corresponding RHS element is an lvalue. Destructuring must not copy those
# elements through a temporary Array first.

plan 3;

{
    my ($x, $y) = 1, 2;
    my (\a, \b) := ($x, $y);
    a = 10;
    b = 20;
    is-deeply ($x, $y), (10, 20),
        'list destructuring binds sigilless names to scalar lvalues';
}

dies-ok {
    my (\a, \b) := (1, 2);
    a = 10;
}, 'binding a literal still produces a readonly sigilless term';

dies-ok {
    my (\a, \b) := (1 + 1, 2);
    a = 10;
}, 'binding a computed value still produces a readonly sigilless term';
