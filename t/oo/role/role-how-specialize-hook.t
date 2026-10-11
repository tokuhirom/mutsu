use Test;

plan 7;

# A `trait_mod:<is>` on a role method runs with $*PACKAGE the individual
# parametric role, whose HOW is a ParametricRoleHOW.
my $how-name;
my $is-prole;
my role Hook {
    method specialize(Mu \r, Mu:U \obj, *@pos, *%named) {
        obj.^add_method('alias', r.^find_method('real'));
        callsame;
    }
}
multi sub trait_mod:<is>(Method:D \meth, :$also!) {
    $how-name = $*PACKAGE.HOW.^name;
    $is-prole = $*PACKAGE.HOW ~~ Metamodel::ParametricRoleHOW;
    $*PACKAGE.HOW does Hook unless $*PACKAGE.HOW ~~ Hook;
}

role Ber { method real is also<x> { 'Str' } }
ok $how-name.starts-with('Perl6::Metamodel::ParametricRoleHOW'), 'role body trait sees ParametricRoleHOW';
ok $is-prole, '$*PACKAGE.HOW ~~ Metamodel::ParametricRoleHOW';

# Composing the role drives the user `specialize` override on its HOW.
class Bar does Ber { }
is Bar.alias, 'Str', 'specialize override added a method to the composing class';
is Bar.real, 'Str', 'the original method is still there';

# A class that does not compose the role is untouched.
class Baz { }
ok !Baz.^can('alias'), 'unrelated class is not affected';

# A role pending multi list is empty (all candidates ride along with add_method).
ok Ber.^multi_methods_to_incorporate ~~ Positional, '^multi_methods_to_incorporate answers a list';
is Ber.^multi_methods_to_incorporate.elems, 0, 'no pending multis';
