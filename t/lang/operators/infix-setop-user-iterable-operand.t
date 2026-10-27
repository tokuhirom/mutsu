use lib 't/lib';
use Test;

# A user `multi infix:<(|)>` (and the other set operators) whose parameters are
# an `Iterable` type is called with the operand itself, not with the Array of
# its elements the built-in operator walks. (Red's `M.^all.grep(...) (|)
# M.^all.grep(...)` answered a `Set` of the rows, never reaching its
# `Red::ResultSeq` candidate.)

use SetopUserInfix;

plan 12;

my $a = rs('a');
my $b = rs('b');

is ($a (|) $b).name, '(a union b)', '(|)';
is ($a ∪ $b).name,   '(a union b)', '∪';
is ($a (&) $b).name, '(a (&) b)',   '(&)';
is ($a ∩ $b).name,   '(a (&) b)',   '∩';
is ($a (-) $b).name, '(a (-) b)',   '(-)';
is ($a (+) $b).name, '(a (+) b)',   '(+)';

my $u = $a (|) $b;
is $u.name, '(a union b)', 'in a declaration initializer';
my $v = ($a ∪ $b).name;
is $v, '(a union b)', 'with a method call in a declaration initializer';

# Operands the candidates do not accept keep the built-in operators.
is-deeply ((1, 2) (|) (2, 3)), set(1, 2, 3), '(|) on lists';
is-deeply ((1, 2) (&) (2, 3)), set(2), '(&) on lists';
is-deeply ((1, 2) (-) (2, 3)), set(1), '(-) on lists';
is-deeply ((1, 2) (+) (2, 3)), bag(1, 2, 2, 3), '(+) on lists';

done-testing;
