use Test;

plan 8;

# In strict mode (the Raku default), assignment never declares. Inside a bare
# block, assigning to a variable that was never declared is X::Undeclared.

throws-like { EVAL '{ $var = 42 }' }, X::Undeclared,
    'assignment to an undeclared scalar inside a block dies';

throws-like { EVAL '{ @arr = 1, 2, 3 }' }, X::Undeclared,
    'assignment to an undeclared array inside a block dies';

throws-like { EVAL '{ %hash = a => 1 }' }, X::Undeclared,
    'assignment to an undeclared hash inside a block dies';

throws-like { EVAL '{ my $var = 42 }; say $var' }, X::Undeclared,
    'a block-local my does not leak to the enclosing scope';

# --- cases that MUST NOT die ---

lives-ok { EVAL 'my $x; { $x = 42 }' },
    'assigning to an outer my inside a block is fine';

lives-ok { EVAL '{ my $y; $y = 42 }' },
    'assigning to a block-local my is fine';

lives-ok { my $z = 1; EVAL '{ $z = 42 }'; $z == 42 or die },
    'assigning to a caller lexical inside an EVAL block is fine';

lives-ok { EVAL '{ $_ = 42 }' },
    'assigning to the topic $_ inside a block is fine';
