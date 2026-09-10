use Test;

# Chaining a structural (non-associative) comparison operator such as `<=>`
# is a compile-time X::Syntax::NonAssociative carrying .left / .right.

plan 5;

throws-like '1 <=> 2 <=> 3', X::Syntax::NonAssociative, left => '<=>', right => '<=>';
throws-like '1 cmp 2 cmp 3', X::Syntax::NonAssociative, left => 'cmp', right => 'cmp';
throws-like '1 leg 2 leg 3', X::Syntax::NonAssociative, left => 'leg', right => 'leg';

# A single comparison is fine, and parenthesized chains are fine.
is (1 <=> 2), Order::Less, 'single <=> works';
lives-ok { my $x = (1 <=> 2); my $y = ($x <=> Order::Same) }, 'parenthesized <=> chain works';
