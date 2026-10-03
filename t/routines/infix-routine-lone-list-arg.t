use Test;

plan 6;

# A lone list operand of a numeric infix routine is numified, not spread.
is infix:<+>($[1,2]), 2, 'infix:<+> numifies a lone itemized array';
is infix:<*>($[3,4]), 2, 'infix:<*> numifies a lone itemized array';
is infix:<->($[3,4,5]), 3, 'infix:<-> numifies a lone itemized array';
is infix:<+>(5), 5, 'one non-list operand is unchanged';
is infix:<+>(), 0, 'no operands gives the identity';
# Operators with a list candidate still join their lone list argument.
is infix:<~>($[1,2]), '12', 'infix:<~> still joins a lone array';
