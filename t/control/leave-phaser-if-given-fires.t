use Test;

plan 6;

# A LEAVE (and, by the same mechanism, ENTER/KEEP/UNDO) phaser directly
# inside an `if`/`given` block never fired at all -- two separate, narrow
# gaps in how their bodies compile:
#
# 1. `if`'s compile-time-constant-condition fold (ADR-0006 §2.2, `if True {
#    ... }` compiling away the runtime jump entirely) routed through
#    compile_resolved_branch_body, which never checked for phasers, unlike
#    the ordinary (non-constant-condition) Stmt::If arm.
# 2. `given`'s body was compiled by iterating and compiling each statement
#    in place; an un-lowered Stmt::Phaser{kind: Leave} alone compiles to a
#    no-op, so its LEAVE never fired regardless of the topic's constness.
#
# See news/2026-08/leave-phaser-if-given-not-firing.md.

{
    my $ran = 0;
    if True {
        LEAVE $ran++;
    }
    is $ran, 1, 'LEAVE inside `if TRUE_CONSTANT` fires (constant-fold path)';
}

{
    my $ran = 0;
    my $cond = 1 + 1 == 2;
    if $cond {
        LEAVE $ran++;
    }
    is $ran, 1, 'LEAVE inside `if` with a non-constant condition still fires (regression guard)';
}

{
    my $ran = 0;
    given 5 {
        LEAVE $ran++;
    }
    is $ran, 1, 'LEAVE inside `given` fires';
}

{
    # A dynamic (PROCESS::) var write in the LEAVE must be visible to code
    # that runs after the block exits -- the shape the bug was originally
    # found through (Log::Timeline's FakeOutput.new -> $output { ... }).
    class FakeOutput { }
    given FakeOutput.new -> $output {
        PROCESS::<$LPTEST> = $output;
        LEAVE PROCESS::<$LPTEST> = Nil;
    }
    nok PROCESS::<$LPTEST>.defined, 'a LEAVE-driven PROCESS:: reset inside `given` is visible after it exits';
}

{
    my $ran = 0;
    if True {
        ENTER $ran++;
    }
    is $ran, 1, 'ENTER inside `if TRUE_CONSTANT` still fires (regression guard, was already working)';
}

{
    # A real if/given BLOCK (not constant-folded, no phasers) is unaffected.
    is (given 5 { 6 }), 6, 'a phaser-free given block is unaffected (regression guard)';
}
