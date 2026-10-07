use Test;

plan 3;

my $caught = '';
try { "abc".subst-mutate("a", "b"); CATCH { default { $caught = .^name } } }
is $caught, 'X::Multi::NoMatch', 'subst-mutate on a literal is X::Multi::NoMatch';

my $s = "abc";
ok $s.subst-mutate("a", "b"), 'subst-mutate on a variable still matches';
is $s, "bbc", 'and mutates the variable';
