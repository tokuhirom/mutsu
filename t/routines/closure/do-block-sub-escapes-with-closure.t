use Test;

# A `sub` declared directly in a `do { }` block is lexical to that block, the
# same way one declared in a statement-position bare block `{ }` is: a
# closure created inside the block and returned/assigned out of it must
# still be able to call the sub after the block has exited.
#
# `OpCode::BlockScope` (the statement form) raises `block_scope_depth` for
# every bare block, which is what makes `RegisterSub` stash an escape-hatch
# copy of the sub under a reserved env key (`BLOCK_LEXICAL_SUB_PREFIX`) for a
# closure that outlives the block's own routine-registry restore.
# `OpCode::DoBlockExpr` (the value-position `do { }`/inline block form) never
# raised that counter, so the escape hatch never engaged and the closure died
# with "Unknown function" once the block exited (#9636).

plan 4;

my $m = do { sub helper { 7 }; -> { helper() } };
is $m(), 7, 'do{} block: a closure returned as the block value calls its own sub after exit';

my &n;
do { sub helper2 { 7 }; &n = -> { helper2() }; 1 };
is n(), 7, 'do{} block: a closure assigned out of the block calls its own sub after exit';

my &n2;
{ sub helper3 { 7 }; &n2 = -> { helper3() } }
is n2(), 7, 'statement-position bare block: same shape keeps working (regression guard)';

class U {
    do {
        sub helper4($x) { $x * 3 }
        ::?CLASS.^add_method("m", method { helper4(21) })
    }
}
is U.new.m, 63, 'do{} block inside a class body: ^add_method-installed method calls its own sub';
