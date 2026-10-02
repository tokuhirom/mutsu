use v6;
use Test;

# `Pkg::<@a> := v` / `Pkg::<%h> := v` is the stash spelling of
# `@Pkg::a := v`: it rebinds the package variable, in statement and
# expression context alike (#10546). Measured against rakudo.

plan 8;

package Q { our @a; our %h; our $s }

Q::<@a> := [5, 6];
is-deeply @Q::a, [5, 6], 'Pkg::<@a> := v rebinds @Pkg::a';
Q::<%h> := { a => 1 };
is-deeply %Q::h, { a => 1 }, 'Pkg::<%h> := v rebinds %Pkg::h';

my $r = (Q::<@a> := [7]);
is-deeply $r, [7], 'expression-form bind returns the bound value';
is-deeply @Q::a, [7], '... and rebinds the variable';
my $h = (Q::<%h> := { b => 2 });
is-deeply %Q::h, { b => 2 }, 'expression-form hash bind';

is (Q::<$s> := 4), 4, 'expression-form scalar bind';
is $Q::s, 4, '... rebinds $Pkg::s';

my @src = 1, 2, 3;
Q::<@a> := @src;
@src.push(4);
is @Q::a.elems, 4, 'a bind aliases the bound array, not a copy';
