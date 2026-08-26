use Test;

# `exec_package_scope_op` (src/vm/vm_misc_reduction_scan.rs) restores the
# mainline `locals` slots a `package`/`module`/`class`-via-`Stmt::Package`
# body might have written through to `env` when it exits. Historically this
# restore loop pulled EVERY declared local's value out of `env`
# unconditionally -- but `env`'s bare keys are pre-seeded with a decl-seed
# placeholder (`Any`) for every mainline lexical before any of them run, and
# mutsu's locals/env dual store only mirrors a plain scalar's writes into
# `env` when the compiler's `needs_env_sync` analysis says something actually
# reads that slot by name. A `module`/`package` body that never even
# mentions an outer lexical still triggered the blanket restore, copying the
# stale placeholder straight over a live, correctly-assigned local:
#   my $x = "top"; module M { }; say $x;   # used to print (Any)
#
# The root cause was two-fold:
#  1. `compute_needs_env_sync` (src/opcode.rs) never scanned `PackageScope`
#     bodies at all (unlike `BlockScope`/`BlockLocalScope`/`ForLoop`/
#     `MakeGather`/`WheneverScope`), so a slot referenced ONLY inside a
#     package/module body never got its env mirror kept live.
#  2. `exec_package_scope_op`'s restore loop read `env` for every slot in the
#     whole compiled unit, not just the ones the executing body could
#     plausibly have written -- there was no reason to trust `env` for an
#     untouched slot at all.
# Fixed by teaching `compute_needs_env_sync` about `PackageScope` (feeding
# `EnvConsumerSlots::package_scope`) and gating the restore loop on
# `code.needs_env_sync`, the same signal the write side already uses to
# decide whether a slot's `env` mirror is current.
#
# This file is a companion to `t/in-file-package-our-var.t`, which pins the
# `our`-linkage side of the same restore loop and no longer needs to warm the
# frame with a leading `class Warm { }` to dodge this bug.

plan 15;

# --- the headline repro: an untouched outer lexical must survive an empty
# package/module block with NO preceding statement to "warm" env. ------------
{
    my $x = "top";
    module M1 { };
    is $x, "top", "empty module does not clobber an untouched outer lexical";
}

{
    my $x = "top";
    package P1 { };
    is $x, "top", "empty package does not clobber an untouched outer lexical";
}

{
    my $x = "top";
    grammar G1 { };
    is $x, "top", "empty grammar does not clobber an untouched outer lexical";
}

# --- same shape, but with an earlier statement that DOES flush env (the
# workaround the bug used to require) -- must keep working too. -------------
{
    class Warm1 { };
    my $x = "top";
    module M2 { };
    is $x, "top", "empty module after an earlier flushed statement is still fine";
}

# --- the write-through case the restore loop exists FOR: a block body that
# assigns to an outer lexical must still have that write observed. ----------
{
    my $x = 1;
    module M3 { $x = 2 };
    is $x, 2, "module body write-through reaches the outer lexical (unwarmed)";
}

{
    class Warm2 { };
    my $x = 1;
    module M4 { $x = 2 };
    is $x, 2, "module body write-through reaches the outer lexical (warmed)";
}

{
    my $x = 1;
    package P2 { $x = 2 };
    is $x, 2, "package body write-through reaches the outer lexical";
}

# --- nested package/module scopes: both a plain nested scope and one whose
# innermost body performs the write. -----------------------------------------
{
    my $x = "top";
    module M5 { module N5 { } };
    is $x, "top", "nested empty modules do not clobber an untouched outer lexical";
}

{
    my $x = "top";
    module M6 { module N6 { $x = "written" } };
    is $x, "written", "a doubly-nested module write-through still reaches the outer lexical";
}

# --- scope-exit paths that do NOT share this bug (they never route through
# exec_package_scope_op at all) -- pinned so a future refactor that merges
# these paths is caught if it regresses. -------------------------------------
{
    my $x = "top";
    class C1 { };
    is $x, "top", "class declaration does not clobber an untouched outer lexical";
}

{
    my $x = "top";
    { };
    is $x, "top", "a bare block does not clobber an untouched outer lexical";
}

{
    my $x = "top";
    BEGIN { };
    is $x, "top", "a BEGIN block does not clobber an untouched outer lexical";
}

# --- a role body is never eagerly executed at declaration time (roles are
# composed lazily), so a write inside one must NOT reach the outer lexical --
# this is a raku semantic, not a mutsu-specific carve-out. -------------------
{
    my $x = 1;
    role R1 { $x = 2 };
    is $x, 1, "a role body is not eagerly run, so it cannot write an outer lexical";
}

# --- several package/module blocks in one unit, only one of which touches
# the lexical: the untouched ones must not clobber it either. ----------------
{
    my $x = "top";
    module M7 { };
    module M8 { $x = "changed" };
    module M9 { };
    is $x, "changed", "an untouched sibling module does not undo an earlier module's write";
}

# --- the positive case the unconditional loop was originally protecting,
# restated with an intervening unrelated declaration. ------------------------
{
    my $x = 1;
    class Unrelated { };
    module M10 { $x = $x + 1 };
    is $x, 2, "write-through still works alongside an unrelated class declaration";
}

done-testing;
