use Test;
use lib $?FILE.IO.parent(2).add('lib').Str;

plan 8;

# An EVAL sees the imports of the blocks around its own call site, not those
# of the blocks open when it runs (#14251).
sub out-sub { (try EVAL 'evc-sub()') // 'gone' }
sub out-postfix { (try EVAL '5!') // 'gone' }
sub out-infix { (try EVAL '1 +++ 2') // 'gone' }
sub out-code { (try EVAL '&evc-sub') ?? 'seen' !! 'gone' }
{
    use EvalChainOps;
    is evc-sub(), 'evc-sub', 'the block sees the import';
    is out-sub(), 'gone', 'a routine outside the block: routine';
    is out-postfix(), 'gone', 'a routine outside the block: postfix operator';
    is out-infix(), 'gone', 'a routine outside the block: infix operator';
    is out-code(), 'gone', 'a routine outside the block: &-variable';

    # ... and an EVAL written inside the block still sees them.
    is (EVAL 'evc-sub()'), 'evc-sub', 'an EVAL in the block sees the routine';
    is (EVAL '5!'), 'fact(5)', 'an EVAL in the block sees the postfix operator';
    sub inner { EVAL '1 +++ 2' }
    is inner(), 'pp(1,2)', 'an EVAL in a routine declared in the block sees them';
}
