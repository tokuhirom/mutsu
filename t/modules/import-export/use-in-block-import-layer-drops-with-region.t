use Test;
use lib 't/lib';

# What a block's `use` imports through the env (a `sub EXPORT` hook's
# variables, term and routine, a lexical type and an enum key) lives in the
# block's import layer: it is dropped with the block on every exit path and
# is not copied back to the enclosing scope (ADR-14140). Checked against
# rakudo.
plan 13;

{
    use HookExportsValues;
    is $hv, 'hv-val', 'the block sees the hook scalar';
    is @hl.join(','), 'a,b', 'the block sees the hook array';
    is ht, 'term-val', 'the block sees the hook term';
    is hf(), 'hf', 'the block sees the hook routine';
    my &c = -> { $hv ~ hf() };
    is c(), 'hv-valhf', 'a closure made in the block keeps them';
    is (try EVAL '$hv'), 'hv-val', 'an EVAL in the block sees them';
}
dies-ok { EVAL '$hv' }, 'the hook scalar is gone after the block';
dies-ok { EVAL 'hf()' }, 'the hook routine is gone after the block';

# A block that dies leaves nothing behind.
try { { use HookExportsValues; die 'boom' } };
dies-ok { EVAL '$hv' }, 'nothing is left after a block that died';

# A routine's own use runs on every call and each call drops its imports.
sub twice { use HookExportsValues; $hv }
is twice() ~ twice(), 'hv-valhv-val', 'every call imports again';
dies-ok { EVAL '$hv' }, 'nothing is left after the calls';

# An inner block's import ends with the inner block, not the outer one's.
{
    use HookExportsValues;
    { use HookExportsValues; Nil }
    is $hv, 'hv-val', 'the outer import survives the inner block';
}
# A deep chain still works: the region flattens the env when it must.
sub deep($n) { use HookExportsValues; $n > 0 ?? deep($n - 1) !! $hv }
is deep(40), 'hv-val', 'an import region inside deep recursion binds';
