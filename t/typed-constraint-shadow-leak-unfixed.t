use v6;
use Test;

# ADR-0042 slice 1 did NOT fix every type-constraint scope leak -- only the
# "fresh-after" shape covered by t/typed-constraint-scope-matrix.t (a typed
# `my` inside a branch/loop, then a FRESH untyped `my` of the same name
# declared AFTER that scope exits).
#
# This file pins the shapes that are still broken, so a future slice has a
# clear before/after instead of the leak silently going untested. Two
# residuals, both recorded in
# todo/deep/scoped-type-declaration-tags-the-shadowed-outer-value.md:
#
# 1. The "outer-first shadow" shape: an outer untyped variable declared
#    BEFORE the inner typed declaration, which SHADOWS it, then reused
#    (not re-declared) after the branch/loop exits. Root cause:
#    `exec_set_var_type` tags whatever value currently sits in `env` under
#    the declared name at the moment the type-constraint op runs, which --
#    since the type op runs before the shadowing declaration's own
#    value-store op -- is still the OUTER value. This corrupts the outer
#    CONTAINER'S OWN embedded metadata (not just a name-keyed side table),
#    so it survives regardless of how the name-keyed metadata is scoped.
#    Confirmed present identically before and after slice 1's four steps,
#    and confirmed to affect if/unless/else EQUALLY to while/loop/repeat/for
#    -- contradicting the ADR's own prediction that step 4
#    (`BlockLocalScope` exit cleanup) would fix if/unless/else here too.
#    Expected values verified against raku (raku accepts every row below).
#
# 2. while/loop/repeat/for bodies compile through
#    `compile_body_with_implicit_try`, which emits no scope-boundary opcode
#    at all (§2.1's original finding, orthogonal to residual 1) -- so even
#    the "fresh-after" shape that IS fixed for if/unless/else could regress
#    for loop bodies once a genuine fresh-after loop-body repro is found.
#    No such repro is known today (every fresh-after loop-body shape tried
#    during the slice-1 session already passed, both before and after slice
#    1), so residual 2 has no live assertion here -- only residual 1 is
#    pinned below, covering both branches and loops via the shadow shape.
#
# TODO: ADR-0042 slice 2/3 -- see the todo/deep file for the fix direction
# (retiming `exec_set_var_type`'s tagging relative to the declaration's own
# value-store, or giving it an explicit "this is a fresh declaration, not a
# retag" signal). Do not delete these `todo`-marked assertions when they
# start failing -- flip them to `dies-ok`/positive assertions and move this
# file's header to record what fixed them.

# `if`/`unless`/`else`: ADR-0042 predicted these would be fixed by slice 1
# step 4. They are not, for this specific (shadow, not fresh-after) shape.
{
    my $x;
    if True { my Str $x = "a"; }
    todo "ADR-0042 slice 1 does not fix the outer-first shadow shape (see todo/deep)";
    lives-ok { $x = 42 }, 'if-branch shadow: outer $x usable after the branch (TODO)';
}

{
    my $x;
    unless False { my Str $x = "a"; }
    todo "ADR-0042 slice 1 does not fix the outer-first shadow shape (see todo/deep)";
    lives-ok { $x = 42 }, 'unless-branch shadow: outer $x usable after the branch (TODO)';
}

{
    my $x;
    if False { } else { my Str $x = "a"; }
    todo "ADR-0042 slice 1 does not fix the outer-first shadow shape (see todo/deep)";
    lives-ok { $x = 42 }, 'else-branch shadow: outer $x usable after the branch (TODO)';
}

# while/loop/repeat/for: the ADR predicted these stay broken. They do.
{
    my $x;
    my $i = 0;
    while $i < 1 { my Str $x = "a"; $i++; }
    todo "ADR-0042 slice 1 does not reach while-body shadow (see todo/deep)";
    lives-ok { $x = 42 }, 'while-body shadow: outer $x usable after the loop (TODO)';
}

{
    my $x;
    loop (my $i = 0; $i < 1; $i++) { my Str $x = "a"; }
    todo "ADR-0042 slice 1 does not reach C-style-loop-body shadow (see todo/deep)";
    lives-ok { $x = 42 }, 'C-style-loop-body shadow: outer $x usable after the loop (TODO)';
}

{
    my $x;
    my $i = 0;
    repeat { my Str $x = "a"; $i++; } while $i < 1;
    todo "ADR-0042 slice 1 does not reach repeat-body shadow (see todo/deep)";
    lives-ok { $x = 42 }, 'repeat-body shadow: outer $x usable after the loop (TODO)';
}

{
    my $x;
    for 1..1 { my Str $x = "a"; }
    todo "ADR-0042 slice 1 does not reach for-body shadow (see todo/deep)";
    lives-ok { $x = 42 }, 'for-body shadow: outer $x usable after the loop (TODO)';
}

# The container twin of the same shadow shape (§2.2 of the ADR): also
# unfixed, for both branches and loops.
{
    my @a;
    if True { my Int @a; @a.push(5); }
    todo "ADR-0042 slice 1 does not fix the outer-first container shadow shape (see todo/deep)";
    lives-ok { @a.push("x") }, 'if-branch container shadow: outer @a usable after the branch (TODO)';
}

{
    my @a;
    my $i = 0;
    while $i < 1 { my Int @a; @a.push(5); $i++; }
    todo "ADR-0042 slice 1 does not reach while-body container shadow (see todo/deep)";
    lives-ok { @a.push("x") }, 'while-body container shadow: outer @a usable after the loop (TODO)';
}

{
    my @a;
    for 1..1 { my Int @a; @a.push(5); }
    todo "ADR-0042 slice 1 does not reach for-body container shadow (see todo/deep)";
    lives-ok { @a.push("x") }, 'for-body container shadow: outer @a usable after the loop (TODO)';
}

done-testing;
