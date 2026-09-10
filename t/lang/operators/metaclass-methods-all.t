use Test;

plan 4;

class ParentForMethodsAll {
    method inherited { 'inherited' }
}
class ChildForMethodsAll is ParentForMethodsAll {
    method own { 'own' }
}

my @ordinary = ChildForMethodsAll.^methods.map(*.name);
my @all = ChildForMethodsAll.^methods(:all).map(*.name);

ok 'own' (elem) @ordinary, 'ordinary .^methods includes the class method';
ok 'inherited' (elem) @ordinary, 'ordinary .^methods includes user-defined ancestors';
ok 'serial' (elem) @all, '.^methods(:all) includes universal Any methods';
ok !('serial' (elem) @ordinary), 'ordinary .^methods does not include universal Any methods';

done-testing;
