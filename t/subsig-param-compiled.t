use Test;

# ADR-0019 C6e-2b: routines with sub-signature (destructuring) parameters run
# through the compiled entry — the OTF/plan-bytecode gate no longer excludes
# them. Binding runs through the shared binder on both arms and destructured
# elements bind read-only, so behavior must match the interpreter arm exactly.

plan 6;

sub pairdest (Pair (Int:D :key($k), Str:D :value($v))) { "$k|$v" }
sub listdest ([$a, $b, *@rest]) { "$a-$b-@rest[]" }
sub nested (Pair (Int:D :key($plan), Pair :value((Str:D :key($desc), :value(&code))))) {
    "$plan/$desc/" ~ code()
}
sub sigilless-dest ((\i, \j)) { i + j }

is pairdest(3 => "x"), '3|x', 'named Pair destructure';
is listdest([1, 2, 3, 4]), '1-2-3 4', 'positional list destructure with slurpy tail';
is nested(2 => ("hi" => { 42 })), '2/hi/42',
    'nested Pair-in-Pair destructure with a code-valued leaf (the group-of shape)';
is sigilless-dest((5, 7)), 12, 'sigilless destructured elements';

# EVAL-boundary call: the compiled routing must hold across a re-entrant
# compile too.
my $p = 4 => "y";
is EVAL('pairdest($p)'), '4|y', 'destructuring callee reached through EVAL';

# Repeated calls rebind fresh destructured elements.
is listdest([9, 8]) ~ ';' ~ listdest([7, 6]), '9-8-;7-6-',
    'sequential calls destructure their own arguments';
