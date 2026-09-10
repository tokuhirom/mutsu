use lib $*PROGRAM.parent.child("lib").Str;
use Test;
use BlockCarrierOtf;

# A `sub` declared inside a bare block/closure compiles to bytecode whose
# `RegisterSub` opcode's compiled-routine key must resolve against the
# block's OWN compiled-functions table. Before this fix, `SubData` (unlike
# `CompiledFunction`/`MethodDef`) carried no such table, so a nested sub's
# `RegisterSub` opcode inherited whatever table the block's CALLER happened
# to have — which is a different, unrelated table when the block is invoked
# from a different compilation unit's own compiled code (e.g. a module sub
# that itself has nested subs, calling a caller-supplied `&block` argument).
# This still worked correctness-wise via the AST-body interpreter fallback;
# this file pins the behavior now that block dispatch runs the compiled body
# with the block's own functions table
# (ADR-0019 C6e-3c, `todo/deep/c6e-legacy-body-drop-blocked-by-gate-rejected-shapes.md`).

plan 5;

is run-it({
    sub pos-match { "matched" }
    pos-match();
}), "matched", 'nested sub inside a block invoked cross-module resolves its own bytecode';

is run-it({
    sub add-one($x) { $x + 1 }
    add-one(41);
}), 42, 'nested sub with a param inside a cross-module block';

my @r = (1, 2, 3).map({
    sub double($x) { $x * 2 }
    double($_);
});
is @r, (2, 4, 6), 'nested sub inside a native .map block';

my @g = (1, 2, 3, 4).grep({
    sub even($x) { $x % 2 == 0 }
    even($_);
});
is @g, (2, 4), 'nested sub inside a native .grep block';

# Repeated cross-module invocation (idempotent re-registration path).
is run-it({
    sub pos-match { "matched-again" }
    pos-match();
}), "matched-again", 'repeated cross-module block invocation with its own nested sub';
