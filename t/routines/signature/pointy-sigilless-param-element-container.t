use Test;

# A pointy block's sigilless parameter binds the argument element's
# container, like a `sub (\t)` does (#14210). Expected values are the
# `raku` output.

plan 7;

my @a = 1, 2, 3;
my $b = -> \t { t = 5 };
$b(@a[0]);
is @a.raku, '[5, 2, 3]', 'pointy block in a scalar writes an array element';

my @c = 1, 2, 3;
my &cv = -> \t { t = 6 };
cv(@c[1]);
is @c.raku, '[1, 6, 3]', 'pointy block in a code variable, called by name';

my @d = 1, 2, 3;
&cv(@d[2]);
is @d.raku, '[1, 2, 6]', 'pointy block in a code variable, called with &';

my %h = a => 1;
$b(%h<a>);
is %h<a>, 5, 'a hash element is written';

my @e = 1, 2;
my $rd = -> \t { t + 1 };
is $rd(@e[0]), 2, 'reading the parameter still works';

my @f = 1, 2;
is-deeply (-> \t { t })(@f[0]), 1, 'an immediate pointy block still returns the value';

my @g = 1, 2;
my $imm = -> \t { t = 9 };
$imm(@g[1]);
is @g.raku, '[1, 9]', 'only the element argument is written';
