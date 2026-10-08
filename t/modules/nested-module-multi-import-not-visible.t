# A proto/multi family a module imports for its own use must not be visible
# to the scope that `use`s that module, through a symbolic `::('&name')`
# lookup either (GH #12161). Rakudo leaves it undeclared.
use lib 't/lib';
use Test;

plan 6;

my $name = '&nested-' ~ 'mexp';
use BlockUseNestedMultiOuter;
nok defined(::($name)), "a nested module's imported multi family is not visible";
is outer-multi-probe(), 'intstr', "the nested module still calls its own import";
ok defined(::('&outer-multi-probe')), "the module's own export is visible";
nok defined(::($name)), 'still hidden after a later statement uses the module';

# A lazy `.map` Seq returned from a subtest block is pulled by Test's own
# routine; the block still resolves the script's imports, not Test's.
use BlockUseLazyMapExp;
subtest 'lazy map pulled by another unit', {
    plan 1;
    (1,).map: { is lazy-mexp(1), 'int', 'imported multi resolves in the map block' };
}
is lazy-mexp('a'), 'str', 'the script still sees the multi it imported';
