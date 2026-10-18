use Test;

# ADR-8751 stage 1: the packages a block-scoped `use` loads are the block's
# lexical import. A package-qualified name under them (`::('M::Foo')`,
# `::('&M::f')`) resolves inside the block, but not after it and not in a
# routine written outside it that the block calls (#13801). Checked against
# rakudo.
plan 10;

use lib 't/lib';

sub callee-type { (try ::('UseInBlockQualifiedPkg::Foo').hi) // 'hidden' }
sub callee-code { (try ::('&UseInBlockQualifiedPkg::osub')()) // 'hidden' }
my &keeps;
{
    use UseInBlockQualifiedPkg;
    is ::('UseInBlockQualifiedPkg::Foo').hi, 'foo', 'an indirect qualified type lookup in the block';
    is ::('&UseInBlockQualifiedPkg::osub')(), 'osub', 'an indirect qualified routine lookup in the block';
    is UseInBlockQualifiedPkg::Foo.hi, 'foo', 'the qualified bareword';
    is UseInBlockQualifiedPkg::own(), 'foo', 'the module\'s own code still sees its packages';
    is callee-type(), 'hidden', 'a callee written outside the block does not see the type';
    is callee-code(), 'hidden', 'nor the routine';
    &keeps = { (try ::('UseInBlockQualifiedPkg::Foo').hi) // 'lost' };
}
is (try ::('UseInBlockQualifiedPkg::Foo').hi) // 'hidden', 'hidden', 'the type is gone after the block';
is (try ::('&UseInBlockQualifiedPkg::osub')()) // 'hidden', 'hidden', 'and so is the routine';
is keeps(), 'foo', 'a closure made in the block keeps the import';
{
    use UseInBlockQualifiedPkg;
    is ::('UseInBlockQualifiedPkg::Foo').hi, 'foo', 'a sibling block\'s `use` grants it again';
}
