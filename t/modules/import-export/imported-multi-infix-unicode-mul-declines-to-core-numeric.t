use lib 't/lib';
use Test;
use OpaqueVecMul;

plan 7;

my $three = OpaqueVecMul.new(c => (1, 2, 3));
my $four  = OpaqueVecMul.new(c => (1, 2, 3, 4));

# An imported `multi infix:<×>` whose `where` clauses decline falls back to the
# core `×`, which numifies through `.Numeric` and so fails for an opaque object.
is $three × $three, 'cross', 'the user candidate answers when its where clauses hold';
dies-ok { $four × $four }, 'declined user candidate: the core × throws for an object';
dies-ok { $three × $four }, 'declined on the right operand only';
dies-ok { $four × 2 }, 'an object times a number';
dies-ok { 2 × $four }, 'a number times an object';
dies-ok { $four * 2 }, 'the ASCII spelling agrees';

# prefix + on an opaque object throws instead of answering 0.
dies-ok { +$four }, 'prefix + on an object whose .Numeric fails throws';
