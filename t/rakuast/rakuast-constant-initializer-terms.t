use Test;

# A `constant` initializer reads terms, it does not declare them: reading
# `pi` in `constant p = pi` must not make the unit's `pi` a shadowed term when
# the tree is lowered. And `~~ ($_)` keeps its parentheses through EVAL.

plan 4;

is-approx Q|constant p = pi; p|.AST.EVAL, pi, 'constant initialised from pi';
is-approx Q|constant t = tau; my $x = tau; $x|.AST.EVAL, tau, 'a later pi/tau read stays the setting constant';
is Q|constant e2 = 5; e2|.AST.EVAL, 5, 'a user constant still evaluates';

my $src = Q|my $r; for (/a/,) { $r = "ab" ~~ ($_) }; $r.^name|;
is $src.AST.EVAL, 'Bool', 'a parenthesised topic RHS stays a plain read';
