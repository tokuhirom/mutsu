# Test exports `proto sub skip`. A block's `use Test` binds it in the block, so
# the core `skip` method/routine is what the code after the block calls.
my (&plan, &is-deeply) = do {
    use Test;
    (&plan, &is-deeply)
};

plan 4;

is-deeply <a b c>.skip(1), <b c>, 'the core method works after the block';
is-deeply skip(1, <a b c>), <b c>, 'the core routine works after the block';
{
    use Test;
    skip 'inside the block, the imported skip is Test\'s', 1;
}
is-deeply skip(2, <a b c>), (<c>,), 'and the core routine again after a second block';
