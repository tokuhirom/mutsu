use lib 't/lib';
use Test;

# An imported user role named like a core type (`Variable`) satisfies a type
# constraint spelled with that name: in `my T $x`, an attribute and a
# parameter, also when the importing module mentions the name as a bare word
# in a method body (#16497, #16579).

use ImportedCoreNamedRoleUser;
use ImportedCoreNamedRole;

plan 7;

my $l = LocalVar.new(:name<a>);
my Variable $v = $l;
is $v.name, 'a', 'my Variable $v = an instance of the imported role';
sub takes(Variable $x) { $x.name }
is takes($l), 'a', 'a parameter constrained by the imported role';
is Assign.new(left => $l).left.name, 'a', 'attribute constrained by the imported role';
is Assign.which.^name, 'ImportedCoreNamedRole::Variable', 'bare word names the imported role';

throws-like { my Variable $w = 42 }, X::TypeCheck::Assignment, 'a non-member is still refused';
is (42 ~~ Int), True, 'core type names keep their meaning';
is ('x' ~~ Int), False, 'and still reject non-members';
