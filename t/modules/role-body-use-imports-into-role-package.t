use lib 't/lib';
use Test;

# Regression: a `use` statement inside a parameterized role's body (executed
# lazily as part of role COMPOSITION, not at role declaration time) imported
# into whatever package happened to be current at the composing call site,
# instead of the role's OWN declaring package. A custom infix operator
# imported that way (RoleUse::CmpOperator's `role-use-cmp`, exported into
# RoleUse::Holder's own body via its own `use`) was then unreachable by name
# from the role's own methods, because `bare_name_packages`'s
# enclosing-package search never found it registered under the role's own
# package -- it landed under the composing class's package instead.
#
# Algorithm::Kruskal (an ecosystem distribution: Algorithm::MinMaxHeap +
# Algorithm::MinMaxHeap::CmpOperator's custom `minmaxheap-cmp` infix, called
# from MinMaxHeap's own private `!bubble-up` method) hit this exact shape.

use RoleUseMaker;

plan 2;

my $holder = make-holder();
$holder.insert(make-item(2));
$holder.insert(make-item(1));
is $holder.compare-first-two(), Order::More,
    "a role's own method can call a custom infix its body `use`d from another module";

# A second, independently-parameterised role composition of the SAME role
# must resolve the operator too -- not just the one that happened to trigger
# the first-ever composition.
my $other-holder = make-holder();
$other-holder.insert(make-item(5));
$other-holder.insert(make-item(5));
is $other-holder.compare-first-two(), Order::Same,
    "a later composition of the same role also resolves the imported operator";
