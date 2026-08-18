use Test;

plan 6;

# A plain write to an EXISTING dynamic variable (`$*x = ...`, or a
# `PROCESS::<$x> = ...` pseudo-stash write, which stores through the same
# `*x`-prefixed env key) mutates the variable's container and must remain
# visible after the enclosing block exits -- only a fresh `my $*x = ...`
# REdeclaration inside a block is genuinely block-scoped. `exec_block_scope_op`
# (src/vm/vm_misc_scope.rs) used to treat ANY `*`-prefixed env key as
# block-scoped unconditionally on block exit, discarding the write and
# reverting to the block-entry value regardless of whether it was declared
# fresh in that block. Found via
# todo/tickets/leave-phaser-process-write-lost-in-loop-body.md (originally
# filed as a narrower "LEAVE inside a loop body" case; the real bug is this
# general one, not specific to LEAVE or loops at all).

{
    PROCESS::<$W1> = 42;
    { PROCESS::<$W1> = 99 }
    is PROCESS::<$W1>, 99, 'a PROCESS:: write from inside a plain bare block persists after it exits';
}

{
    PROCESS::<$W2> = 42;
    for 1 { LEAVE PROCESS::<$W2> = 99; }
    is PROCESS::<$W2>, 99, 'a PROCESS:: write from a for-loop LEAVE phaser persists after the loop exits';
}

{
    PROCESS::<$W3> = 42;
    my $done = 0;
    while $done < 1 { LEAVE PROCESS::<$W3> = 99; $done++; }
    is PROCESS::<$W3>, 99, 'a PROCESS:: write from a while-loop LEAVE phaser persists after the loop exits';
}

{
    my $*x = 1;
    { $*x = 99; }
    is $*x, 99, 'a plain reassignment of an existing my $*x from inside a block persists after it exits';
}

{
    my $*x = 1;
    sub reads_x() { $*x }
    { my $*x = 99; is reads_x(), 99, 'my $*x redeclaration is visible inside the block'; }
    is $*x, 1, 'a fresh my $*x redeclaration inside a block still reverts after it exits (regression guard)';
}
