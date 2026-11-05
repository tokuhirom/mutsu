use Test;

# A List literal's items are the variables' own containers, and a multi
# dispatches on the native type of a container argument: ($b, 5)[0] is `byte`,
# an Array copy or a sub return is a plain `Int` (ADR-15309, from Net::BGP's
# `n16(($b1, $b0))`). Every row is what `raku` answers.

plan 14;

multi m(byte $a) { 'byte' }
multi m(Int $a)  { 'Int' }

my byte $b = 1;
my $l = ($b, $b);

is m($b), 'byte', 'the variable itself';
is m(($b, 5)[0]), 'byte', 'List literal item 0';
is m(($b, 5)[1]), 'Int', 'List literal item 1 (a literal)';
is m($l[0]), 'byte', 'a scalar holding the List';
is m($l.head), 'byte', '.head';
is m($l.first), 'byte', '.first';

my @bound := ($b, $b);
is m(@bound[0]), 'byte', 'a List bound with :=';

my @arr = ($b, $b);
is m(@arr[0]), 'Int', 'an Array copy drops the descriptor';
is m([$b][0]), 'Int', 'an Array literal item';
sub id($x) { $x }
is m(id($b)), 'Int', 'a sub return decontainerizes';

my uint8 $u = 3;
is m(($u,)[0]), 'byte', 'uint8 is the same native family';

# The dispatch cache must key the container's native type: one call site, both
# orders.
my @order;
for 0, 1, 1, 0 -> $i { @order.push: m(($b, 5)[$i]) }
is @order.join(','), 'byte,Int,Int,byte', 'one call site, both element kinds, both orders';

# Net::BGP's shape.
multi n16(@a where @a.elems == 2 --> Int) { n16(@a[0], @a[1]) }
multi n16(byte $x, byte $y --> Int)       { $x × 2⁸ + $y }
my byte $b0 = 0; my byte $b1 = 1;
is n16(($b1, $b0)), 256, 'n16 over a List of native variables';
is n16($b1, $b0), 256, 'n16 over the variables directly';
