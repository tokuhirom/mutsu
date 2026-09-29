use v6;
use Test;

plan 4;

my $role := Metamodel::ConcreteRoleHOW.new_type(name => 'ConcreteMopRole');
is $role.HOW.^name, 'Perl6::Metamodel::ConcreteRoleHOW',
    'new_type preserves the concrete role metaclass';
is $role.^roles.gist, '()', 'a new concrete role starts with no composed roles';
is $role.^compose.^name, 'ConcreteMopRole', 'compose returns the role type';
is $role.^roles.gist, '()', 'the composed role still has no composed roles';
