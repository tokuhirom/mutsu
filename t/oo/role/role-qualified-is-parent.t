use Test;
use lib 't/lib';
use QualifiedRoleIsParent;

# Red 0.2.5 declares its SubModelHOW as a unit role inheriting from the
# qualified native metamodel type `Metamodel::SubsetHOW`.  Both block and unit
# role declarations must consume the complete qualified name after `is`.

plan 4;

role BlockQualifiedRole is Metamodel::SubsetHOW { }
ok BlockQualifiedRole.^name eq 'BlockQualifiedRole',
    'a block role accepts a qualified `is` parent';
ok BlockQualifiedRole.^parents.elems == 0,
    'the qualified parent does not become a composed role parent';

ok QualifiedRoleIsParent.^name eq 'QualifiedRoleIsParent',
    'a unit role accepts a qualified `is` parent';
ok QualifiedRoleIsParent.^parents.elems == 0,
    'a unit role keeps the qualified parent as metamodel inheritance';
