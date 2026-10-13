use Test;

# From AttrX::Mooish (and Async::Workers, Cooklang, OO::Plugin): an attribute
# trait handler reads the package being declared as `$*PACKAGE`, as a method
# trait handler does.

plan 4;

my @seen;

multi trait_mod:<is>(Attribute:D $attr, :$tagged!) {
    push @seen, $*PACKAGE.^name;
    push @seen, $*PACKAGE.HOW.^name;
}

my class Holder {
    has $.a is tagged;
}
is @seen[0], 'Holder', 'the class being declared is $*PACKAGE in a class trait';
is @seen[1], 'Perl6::Metamodel::ClassHOW', 'with its own HOW';

# A role's attribute trait sees the role (its HOW is a ParametricRoleHOW).
my @roles;
multi trait_mod:<is>(Attribute:D $attr, :$marked!) {
    given $*PACKAGE.HOW {
        when Metamodel::ParametricRoleHOW { @roles.push: 'role' }
        when Metamodel::ClassHOW { @roles.push: 'class' }
        default { @roles.push: 'other' }
    }
}
my role Marked { has $.m is marked }
my class UsesMarked does Marked { has $.c is marked }
is @roles.sort.join(','), 'class,role', 'a role attribute trait sees a ParametricRoleHOW package';
is @roles.elems, 2, 'and runs once for the role and once for the class';
