use Test;
use lib 't/lib';

# A term imported lexically by a block-scoped `use` shadows a same-named type
# of the file inside that block, and only there (#9963). The EC distribution's
# `t/secp256k1.t` is the shape that broke: a file-scope `grammar G` and a
# `for ... { use secp256k1; ... G ... }` whose `G` is the module's exported
# curve generator. mutsu resolved the bareword to the grammar type object.

plan 8;

grammar G { token TOP { a } }

is G.^name, 'G', 'before the block, G is the grammar';
ok G.parse('a'), 'the grammar parses before the block';

{
    use BlockUseTermShadowsType;
    is G.x, 7, 'inside the block, G is the imported constant';
    isa-ok G, BlockUseTermShadowsType::Point, 'the constant, not the grammar type object';
}

is G.^name, 'G', 'after the block, G is the grammar again';

# The same bareword spelling in a loop body: the per-site type memo must not
# answer the type object once the import is live.
my @seen;
for ^3 {
    @seen.push: G.^name;
    {
        use BlockUseTermShadowsType;
        @seen.push: G.x;
    }
}
is-deeply @seen, ['G', 7, 'G', 7, 'G', 7], 'the import shadows the type on every iteration';

# The hoisted BEGIN-time load of a nested `use` does not import the constant
# into the rest of the file.
sub outside { G.^name }
is outside(), 'G', 'a routine outside the block still sees the grammar';

{
    use BlockUseTermShadowsType;
    is (G.scale(3)).x, 21, 'the imported constant is usable as a value';
}
