use Test;
plan 11;

# A plain two-operand builtin operator called through a code value takes a
# pre-resolved route (#15847); every override the full path honours must still win.
my $cmp = &[cmp];
my $eq  = &[eq];
my $plus = &[+];
is $cmp.(1, 2), Less, 'cmp through a code value';
is $eq.("a", "a"), True, 'eq through a code value';
is $plus.(2, 3), 5, '+ through a code value';
is &infix:<==>(2, 2), True, '&infix:<==> by name';
is &infix:<min>(3, 2), 2, 'min by name';

# A lexical routine under the operator's name is found through the env, not the table.
{
    my sub infix:<+>($a, $b) { 42 }
    is &infix:<+>(1, 2), 42, 'a lexical infix:<+> wins for the name form';
}
is &infix:<+>(1, 2), 3, 'and is gone outside its scope';

# A user multi for a new type, and a candidate declared after the code value exists.
class Box { has $.v }
multi sub infix:<eq>(Box $a, Box $b) { $a.v == $b.v }
is $eq.(Box.new(:v(1)), Box.new(:v(1))), True, 'a user multi candidate is reached after the call site ran';
is $eq.("x", "y"), False, 'builtin candidates still answer';


# Junction and Range operands keep their rules.
ok ($cmp.(1..3, 1..3) === Same), 'cmp on Ranges';
is ($plus.(1|2, 3)).perl.chars > 0, True, 'junction operand is accepted';
