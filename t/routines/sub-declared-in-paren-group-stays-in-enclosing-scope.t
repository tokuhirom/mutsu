use Test;
plan 5;

# A sub declared inside a parenthesised statement group, or the item-context
# `$( ... )`, belongs to the enclosing scope: neither is a block. A real
# `do { }` block, still is one.
my \x = $( sub foo { "foo-ok" }; 2 );
is x, 2, 'the group yields its last value';
is foo(), 'foo-ok', 'a sub declared in $( ... ) is visible afterwards';

my $y = ( sub bar { "bar-ok" }; 3 );
is bar(), 'bar-ok', 'a sub declared in a paren group is visible afterwards';

my $z = $( sub infix:<plus2>($a, $b) { $a + $b + 2 }; 1 );
is 1 plus2 2, 5, 'a user infix declared in $( ... ) is usable afterwards';

do { sub inner-only { 1 } };
throws-like 'inner-only()', X::Undeclared::Symbols, 'a sub in a real do { } block stays local';

