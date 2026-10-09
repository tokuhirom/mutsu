use v6;
use Test;

plan 3;

my @seen;
my multi sub trait_mod:<is>(Method:D $m, :$zz!) {
    @seen.push: $m.package.HOW.^name;
}
role A { method b is zz { } }
class C { method c is zz { } }

is @seen[0], 'Perl6::Metamodel::ParametricRoleHOW', 'role method trait sees a role HOW';
is @seen[1], 'Perl6::Metamodel::ClassHOW', 'class method trait sees ClassHOW';
is A.HOW.^name, 'Perl6::Metamodel::ParametricRoleGroupHOW', 'registered role HOW unchanged';
