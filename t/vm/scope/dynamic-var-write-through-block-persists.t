use Test;

plan 12;

# A plain write to an EXISTING dynamic variable (`$*x = ...`, or a
# `PROCESS::<$x> = ...` pseudo-stash write, which stores through the same
# `*x`-prefixed env key) mutates the variable's container and must remain
# visible after the enclosing block exits -- only a fresh `my $*x = ...`
# REdeclaration inside a block/closure is genuinely scoped to it. Three
# separate restoration mechanisms used to disagree on this:
#   1. `exec_block_scope_op`'s env-restore loop (src/vm/vm_misc_scope.rs)
#      blanket-reverted ANY `*`-prefixed env key on block exit.
#   2. The same function's local-slot restore loops had the identical blanket
#      rule.
#   3. The inline `.map`/`.grep` loop (`eval_map_over_items`,
#      src/runtime/resolution_map_grep.rs) ran the block body in the CALLER's
#      env and did NOT include `my $*x` redeclarations (nor their `$*x` twin
#      env keys) in its save/restore list, so the LAST iteration's fresh
#      `my $*CWD = $_` leaked into the caller (roast S32-io/indir.t test 76)
#      -- masked, before fixes 1/2, by the blanket rules "self-healing" the
#      leak at the next block exit.
# All three now agree on the block_declared / dynamic_declared_sym ownership
# test. See todo/deep -> news/2026-08/dynamic-var-write-lost-on-block-exit.md.

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

# --- the .map inline-loop mechanism (mechanism 3) ---

{
    my $*x = 1;
    my @r = ^3 .map: { my $*x = $_; $_ };
    is $*x, 1, 'a my $*x redeclaration inside a .map block does not leak into the declaring caller';
    is @r.join(','), '0,1,2', '.map with a my $*x redeclaration still maps correctly';
}

{
    my $*x = 1;
    my @r = ^3 .map: { $*x = $_; $_ };
    is $*x, 2, 'a plain $*x write-through inside a .map block propagates to the declaring caller';
}

{
    my @r = ^3 .map: { my $*mydyn = $_; $_ };
    my $v = try { $*mydyn };
    nok $v.defined, 'a my $*mydyn declared only inside a .map block is not resolvable in the outer scope after the map';
}

# --- the .map/start/indir concurrency shape (reduced from roast S32-io/indir.t) ---
# The last iteration's `my $*CWD = $_` must never leak into the mainline,
# which never redeclared $*CWD: after the map, $*CWD is still the untouched
# process-level CWD (raku-verified), not an Int from any iteration.

{
    my $before-CWD = $*CWD;
    my $correct-CWD = "/tmp".IO;
    my int $failures;
    $failures += [+] await flat ^50 .map: {
        my $*CWD = $_;
        my $prom = start indir :!d, $correct-CWD, {
            my $res = $*CWD !~~ $correct-CWD; $*CWD = 42; $res
        }
        $failures++ unless $*CWD eq $_;
        $prom
    }
    is $failures, 0, 'each .map iteration sees its own my $*CWD, under start concurrency';
    is $*CWD, $before-CWD, 'the mainline $*CWD is untouched after .map/start/indir with per-iteration my $*CWD';
}
