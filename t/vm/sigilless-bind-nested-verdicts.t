use Test;

# A sigilless bind's writability verdict travels on the stack from
# `MarkSigillessBindSource` (before the store) to `MarkSigillessBind` (after
# it), so a declaration nested in another's initializer, a loop, or a chain of
# binds keeps its own verdict (ADR-10779 D3). Outputs checked against rakudo.

plan 6;

my @a = 1, 2;
my \x := do { my \inner := @a[1]; inner = 8; 5 };
is-deeply @a, [1, 8], 'a nested element bind writes through';
throws-like { x = 1 }, X::Assignment::RO, 'the outer value bind stays immutable';

my @b = 1, 2;
my \y := do { my \lit := 5; @b[0] };
y = 7;
is-deeply @b, [7, 2], 'an outer element bind stays writable after a nested value bind';

my @c = 1, 2;
for ^2 { my \e := @c[$_]; e = e * 10 }
is-deeply @c, [10, 20], 'a loop rebinds a writable element each iteration';

my %h = a => 1;
my \v := %h<a>;
my \w := v;
w = 5;
is-deeply %h, %(a => 5), 'a bind of a writable sigilless term is writable';

my \z := 3;
throws-like { z = 4 }, X::Assignment::RO, 'a literal bind is immutable';
