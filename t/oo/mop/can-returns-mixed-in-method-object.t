use Test;

# `.^can` answers the same Method object `.^find_method` does, so a role mixed
# into the method (`$m does R`) is visible through it (hide-methods filters the
# `.^can` list with `~~ MethodWrapped`).
plan 4;

my role Marker { has $.hidden is rw }
class A { method bar { 1 } }
class B is A { method bar { 2 } }

my $m := B.^find_method('bar');
$m does Marker;
$m.hidden = True;

ok B.^can('bar')[0] ~~ Marker, "^can returns the mixed-in method object";
nok B.^can('bar')[1] ~~ Marker, "the inherited candidate is unaffected";
is B.^can('bar').grep({ $_ !~~ Marker || !.hidden }).elems, 1, "hidden candidate filtered";
is B.^can('bar').elems, 2, "both candidates listed";
