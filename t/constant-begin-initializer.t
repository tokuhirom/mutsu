use Test;

plan 4;

# `constant X = BEGIN ...` used to die "Cannot assign to a readonly variable":
# the phaser-reordering pass (src/runtime/phasers.rs, reorder_at_level) splits
# a VarDecl containing a nested BEGIN/CHECK/INIT PhaserExpr into a bare
# hoisted declaration plus a separate Assign at its original position, so
# CHECK/INIT phasers can see variables declared later in source order. The
# compiler marks a `constant`'s local slot readonly unconditionally at the
# end of compiling its VarDecl, so the bare hoisted `constant X;` was already
# readonly by the time the split-out `X = ...` assign ran — and a plain
# Assign, unlike a VarDecl's own store, does not bypass the readonly check.
# `constant` declarations are now compiled as one unsplit statement instead.

constant E = BEGIN 5;
is E, 5, 'constant with a rvalue BEGIN initializer';

my constant F = BEGIN 5;
is F, 5, '"my constant" with a rvalue BEGIN initializer';

constant G = BEGIN { 5 };
is G, 5, 'constant with a block-form BEGIN initializer';

# A plain constant co-located in the same block as a BEGIN-initialized one
# must not be affected by the reordering either (both are constant VarDecls
# in the same statement list).
constant H = 10;
is H, 10, 'a plain constant sharing a block with a BEGIN-initialized constant';
