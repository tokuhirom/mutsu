use Test;

# Trailing comma inside parentheses after a list assignment used to be a parse
# error (the list-assignment RHS comma-list parser did not treat ')' as a valid
# trailing-comma terminator).

plan 8;

is (my @a = 1, 2, 3,).raku, '[1, 2, 3]', 'array list-assign with trailing comma in parens';
is (my @b = 1,).raku, '[1]', 'single-element array list-assign with trailing comma in parens';
is (my @c = 1, 2, 3).raku, '[1, 2, 3]', 'array list-assign without trailing comma still works';
is (my %h = a => 1, b => 2,).raku, '{:a(1), :b(2)}', 'hash list-assign with trailing comma in parens';

# Scalar item assignment: the comma is a separate list element, not absorbed.
is (my $x = 5,).raku, '(5,)', 'scalar item-assign keeps trailing comma as list';
is (my $y = 42, 1).raku, '(42, 1)', 'scalar item-assign comma list in parens';

# Bare parenthesized list with trailing comma (already worked, guard against regression).
is (1, 2, 3,).raku, '(1, 2, 3)', 'bare parenthesized list with trailing comma';

my @d = 1, 2, 3,;
is @d.raku, '[1, 2, 3]', 'statement-level list-assign with trailing comma';
