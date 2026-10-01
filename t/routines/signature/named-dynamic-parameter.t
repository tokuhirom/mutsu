use Test;

plan 5;

sub inner { $*bli }

my &f = -> :$*bli { inner };
is f(:bli(111)), 111, 'pointy block :$*bli accepts :bli and binds $*bli for nested calls';

sub g(:$*x) { inner-x() }
sub inner-x { $*x }
is g(:x(5)), 5, 'sub :$*x binds $*x for nested calls';

sub h(:$*x) { $*x }
is h(:x(9)), 9, 'named dynamic parameter is visible in its own body';

my &q = -> :$*y { 1 };
throws-like { q(:z(1)) }, X::AdHoc, 'unknown named argument is still rejected';

my &p = -> :$plain, :$*dyn { "$plain/{inner-dyn()}" };
sub inner-dyn { $*dyn }
is p(:plain<a>, :dyn<b>), 'a/b', 'mixed plain and dynamic named parameters';
