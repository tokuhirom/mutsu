use Test;

plan 5;

ok Empty === (Any andthen 2), 'andthen returns Empty when lhs is undefined type object';
is (S/a/A/ andthen S/b/B/ given "ab"), "AB", 'andthen evaluates rhs with lhs as topic';
is-deeply infix:<andthen>(Empty, 42), Empty, 'infix andthen keeps Empty positional arg';
cmp-ok infix:<andthen>( %(:42a, :72b) ), 'eqv', :42a.Pair | :72b.Pair,
    'infix andthen flattens single hash arg like slurpy';
is-deeply infix:<andthen>([42, 70]), (42 andthen 70),
    'infix andthen flattens single iterable arg';
