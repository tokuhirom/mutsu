use v6;
use Test;

# Regression (#7954, the `expected statement ...` parse-failure index): the
# `&( ... )` contextualizer takes a CIRCUMFIX as its operand — a whole
# parenthesized group, so it may hold a comma list — and contextualizers STACK,
# so `&&(...)` is `&(&(...))`. mutsu parsed only a single expression inside
# `&(`, which stopped at the first comma and failed at the `)`, and had no
# branch for a second `&` at all, so a statement opening with `&&(...)` was
# unparsable (TOML, Clu: `&&($pos >= nqp::chars($t) || ...)`).

plan 12;

# The operand is a circumfix, so a comma list is in scope.
is &(1, 2).^name, 'List', '&(1, 2) is a List';
is &(1, 2).elems, 2, '&(1, 2) keeps both elements';
is &().^name, 'List', '&() is the empty list';

# Contextualizers stack: a second sigil takes the first as its operand.
is &&(0, 1).^name, 'List', '&&(0, 1) stacks the contextualizer';
is &&(0, 1).elems, 2, '&&(0, 1) keeps both elements';
is &&&(1, 2).elems, 2, 'a third & stacks too';

sub seven() { 7 }
is (&&seven)(), 7, '&&seven is the code object, callable';
is (&&seven).^name, 'Sub', '&&seven names a Sub';

# The single-operand forms keep working: `&( ... )` performs no coercion of
# its own, exactly as rakudo's does.
is &("x"), 'x', '&("x") is the string';
is &(&seven).(), 7, '&(&seven) is the code object';
my @a = 1, 2;
is &(@a).^name, 'Array', '&(@a) is the array';

# `&&` in infix position is still the logical-and operator, not a term.
is (3 && (4, 5)).elems, 2, '&& after a term is still infix logical and';
