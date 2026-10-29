use Test;
use lib 't/lib';

# A `use` of a `sub EXPORT` hook module nested in a block of an EVAL'd unit is
# pre-run ahead of the unit's CHECK-time validation. What the hook returns
# binds in the lexical import set of that block (ADR-8751, #14140), so a
# routine it hands out is not callable from outside the block.
plan 4;

is EVAL(q[{ use EvalExportHookUpper; shout("a") }]), 'A', 'a hook routine in its block';
dies-ok { EVAL q[{ use EvalExportHookUpper; shout("a") }; shout("b")] },
    'and not after the block';
is EVAL(q[{ use EvalExportHookUpper; Thing }]), 42, 'a capitalised plain value the hook returns';
is EVAL(q[if True { use EvalExportHookTerm <A B>; B.k }]), 2, 'a term named by the use arguments';
