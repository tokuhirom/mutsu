use v6.d;
use Test;

plan 11;

# A `:=` in an EVAL whose target is a lexical of the scope around it is
# refused while compiling the EVAL (#14657); a variable the EVAL declares
# itself still binds.
my $x = 1;
throws-like { EVAL q[$x := 5] }, X::Bind, 'binding an outer scalar',
    message => 'Cannot use bind operator with this left-hand side';
is $x, 1, '... leaves it alone';

my @a = 1;
sub f { EVAL q[@a := [5]] }
throws-like { f() }, X::Bind, 'binding an outer array from a routine';
is @a.raku, '[1]', '... leaves it alone';

my %h = a => 1;
throws-like { EVAL q[%h := {b => 2}] }, X::Bind, 'binding an outer hash';

my @c = 1;
EVAL q[my @b := @c; @b[0] = 3];
is @c.raku, '[3]', 'an EVAL may bind a variable it declares';

my $y = 1;
EVAL q[my $z := $y; $z = 9];
is $y, 9, '... to an outer variable as the source';

EVAL q[$x = 7];
is $x, 7, 'assigning an outer scalar is still fine';

EVAL q[sub g($p is copy) { $p := 4; $p }; is g(1), 4, 'a parameter bound in its own routine'];
EVAL q[for 1..2 -> $i { my $j := $i; }; pass 'a loop body binds its own variable'];
EVAL q[my $own; $own := 5; is $own, 5, 'declared in the EVAL, bound later'];
