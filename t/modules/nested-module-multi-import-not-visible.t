# A proto/multi family a module imports for its own use must not be visible
# to the scope that `use`s that module, through a symbolic `::('&name')`
# lookup either (GH #12161). Rakudo leaves it undeclared.
use lib 't/lib';
use Test;

plan 4;

my $name = '&nested-' ~ 'mexp';
use BlockUseNestedMultiOuter;
nok defined(::($name)), "a nested module's imported multi family is not visible";
is outer-multi-probe(), 'intstr', "the nested module still calls its own import";
ok defined(::('&outer-multi-probe')), "the module's own export is visible";
nok defined(::($name)), 'still hidden after a later statement uses the module';
