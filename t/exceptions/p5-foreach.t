use Test;

plan 6;

# Perl 5 `for my $x (LIST) { }` foreach syntax is rejected.
throws-like 'for my $x (1, 2, 3) { }', X::Syntax::P5;
throws-like 'for my @x (1, 2, 3) { }', X::Syntax::P5;
throws-like 'LABEL: for my $x (1, 2, 3) { }', X::Syntax::P5;

# Idiomatic Raku for-loops are unaffected.
my $sum = 0;
for 1, 2, 3 -> $x { $sum += $x }
is $sum, 6, 'pointy-block for-loop works';

my @a = 4, 5, 6;
my $s2 = 0;
for @a { $s2 += $_ }
is $s2, 15, 'topic for-loop works';

# `for my $x { }` without a parenthesized list is not Perl 5 syntax.
lives-ok { EVAL 'for (1, 2) { }' }, 'parenthesized list without my is fine';
