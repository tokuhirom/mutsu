use Test;

# An enum VALUE reports its enum TYPE name from `.^name`, not the underlying
# int/str storage type. Previously `Bob.^name` returned "Int" because the
# caret-name dispatch fell through to `value_type_name`, which reports the
# int-backed representation.

plan 8;

enum Names <Bob Alice Carol>;
is Bob.^name, 'Names', 'enum value .^name is the enum type name';
is Alice.^name, 'Names', 'another enum value .^name';

my $x = Bob;
is $x.^name, 'Names', '.^name through a variable';

is Names.^name, 'Names', 'enum TYPE object .^name is unchanged';

# The underlying int view is still available via .Int / .value.
is Bob.Int, 0, '.Int still gives the underlying integer';
is Alice.value, 1, '.value still gives the underlying value';

# Named (index-valued) enums.
enum Color (red => 1, green => 2);
is green.^name, 'Color', 'named enum value .^name';

# .^name maps over a list of enum values.
is-deeply (Bob, Alice).map(*.^name).List, ('Names', 'Names'), '.^name via map';

done-testing;
