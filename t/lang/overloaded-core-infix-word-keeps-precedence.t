use Test;

# Pop (Pop::Point: `multi infix:<cmp> (Pop::Point \a, Pop::Point \b)`, tested as
# `$b + 1 cmp $b + 1`): declaring a multi of a core infix word adds a candidate
# to the core operator and keeps its precedence. mutsu took the declared word
# for a new operator with the trait-less additive level, so the expression read
# as `$b + (1 cmp $b) + 1`.

plan 8;

class P { has $.v }
multi infix:<cmp> (P \a, P \b) { a.v <=> b.v }
multi infix:<+> (P \a, Int \b) { P.new(v => a.v + b) }

my $p = P.new(v => 1);
is $p + 1 cmp $p + 1, Same, 'cmp stays looser than + after a multi of cmp';
is ($p + 1) cmp ($p + 2), Less, 'and the overloaded candidate is the one called';
is 1 + 1 cmp 3, Less, 'the core candidate still serves other operands';

multi infix:<min> (P \a, P \b) { a.v <= b.v ?? a !! b }
is (P.new(v => 2) min P.new(v => 5)).v, 2, 'an overloaded min dispatches to its candidate';
is 2 + 1 min 4 - 1, 3, 'min stays looser than + and -';

multi infix:<x> (P \a, Int \n) { 'px' }
is $p x 2, 'px', 'an overloaded x dispatches to its candidate';
is 2 + 1 x 2, '33', 'x stays looser than + after a multi of x';

multi infix:<leg> (P \a, P \b) { Order::More }
is 1 + 1 leg 3 - 1, Same, 'leg keeps its precedence next to +';
