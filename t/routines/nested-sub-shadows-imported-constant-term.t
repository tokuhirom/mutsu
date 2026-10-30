use Test;
use lib 't/lib';
use ShadowTermConst;

# A `sub phi` declared in a routine is what `phi(...)` calls there, even though
# a `use`d module exports a constant term of the same name.
plan 3;

sub direct($x) {
    sub phi($a, $b) { $a + $b }
    phi($x, 2)
}
is direct(5), 7, 'the nested sub is called from its declaring routine';

sub sibling($x) {
    sub phi($a, $b) { $a + $b }
    sub inner($n) { phi($n, 1) }
    inner($x)
}
is sibling(5), 6, 'and from a sibling nested sub';

is phi, 1.5, 'the imported constant is still the bare term outside';
