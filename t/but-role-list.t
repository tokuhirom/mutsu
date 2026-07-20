use Test;

# `1 but (R1, R2)` composes EVERY role in the parenthesized list as a proper
# role mixin, so `.^name` renders `Int+{R1,R2}` and every role method is
# reachable. Previously the tuple path applied each element as a plain value
# mixin, dropping the roles entirely (`.^name` was just `Int`).

plan 8;

role R1 { method m { 'm!' } }
role R2 { method n { 'n!' } }

my $x = 1 but (R1, R2);
is $x.^name, 'Int+{R1,R2}', 'both roles show in the mixin name';
is $x.m, 'm!', 'R1 method reachable';
is $x.n, 'n!', 'R2 method reachable';
is $x + 4, 5, 'still numifies as the base Int';

# A single role in parens is equivalent to the bare form
my $y = 2 but (R1,);
is $y.^name, 'Int+{R1}', 'single-role list composes R1';
is $y.m, 'm!', 'single-role list method reachable';

# Three roles
role R3 { method o { 'o!' } }
my $z = 3 but (R1, R2, R3);
is $z.^name, 'Int+{R1,R2,R3}', 'three roles compose';
is $z.o, 'o!', 'R3 method reachable';
