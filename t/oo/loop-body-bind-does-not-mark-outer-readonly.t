use Test;

plan 8;

# A `my $x := value` in a loop or branch body binds the body's own `$x`; an
# enclosing `my $x` of the same name stays assignable afterwards.
my $range = 1;
my $list = 1;
my $while = 1;
my $cstyle = 1;
my $branch = 1;
my $outer = 1;

for 1..2 { my $range := "x$_"; }
for <p q> { my $list := "x$_"; }
my $i = 0;
while $i++ < 2 { my $while := "y"; }
loop (my $j = 0; $j < 2; $j++) { my $cstyle := "z"; }
if True { my $branch := 5 }

lives-ok { $range = 2 }, 'after a for over a range';
lives-ok { $list = 2 }, 'after a for over a list';
lives-ok { $while = 2 }, 'after a while loop';
lives-ok { $cstyle = 2 }, 'after a C-style loop';
lives-ok { $branch = 2 }, 'after an if branch';

# A bind to an OUTER variable inside the body still binds it for good.
for 1 { $outer := 7 }
dies-ok { EVAL '$outer = 8' }, 'an outer variable bound in the body stays bound';

# An enclosing binding that is readonly stays readonly after a body that
# declares a same-named writable variable.
my $bound := "lit";
for 1..2 { my $bound = 5; $bound = 6 }
dies-ok { EVAL '$bound = 1' }, 'an enclosing bound literal keeps its readonly mark';
my $free = 1;
for 1..2 { my $free := "q" }
lives-ok { $free = 2 }, 'and a writable one stays writable';
