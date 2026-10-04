use Test;
use lib 't/lib';
use RoleGroupUnits :dpi;

# From CSS::TagSet: `96dpi` mixes in the parametric candidate of a role group
# declared in a module; its methods must dispatch (role ids minted in a
# module's content session have the high bit set).
plan 3;

my $y = 5 but RoleGroupUnits['D', 'px'];
is $y.units, 'px', 'but with a parametric role group member from a module';

my $x = 96dpi;
is $x.units, 'dpi', 'postfix op in the module mixes in the parametric candidate';
is $x.dimension, 'res', 'second method of the candidate';
