use v6;
use Test;

# A captured typed scalar is boxed into a ContainerRef so a block's compound
# assignment can write back to the declaring scope. The cell must retain the
# declared constraint when the compound operator dispatches to a user infix.

plan 2;

class V {
    has $.n;
}

subset Unit of V where { .n == 1 };
multi infix:<+>(V $a, V $b) { V.new(n => $a.n + $b.n) }

my Unit $u = V.new(n => 1);
my $block = { $u += V.new(n => 5) };

throws-like { $block() }, X::TypeCheck::Assignment,
    'a user-infix compound assignment checks the captured scalar type';
is $u.n, 1, 'the rejected compound assignment does not write through the cell';
