use Test;

# Regression pin for issue #8645, Case B: a `$*`-twigil dynamic variable
# (`$*OUT`) is stored under two env keys, the sigilless form (`*OUT`) and the
# sigilled form (`$*OUT`). A fresh `my $*OUT = ...` redeclaration inside a
# block is block-scoped and must be reverted on block exit -- but the
# block-exit restore in `vm_misc_scope.rs` recorded only the SIGILLESS
# spelling in `block_declared_vars` (`exec_set_var_dynamic_op` interns
# whatever name the compiler emits, which for a chained declaration like
# `$out = my $*OUT = Foo.new` -- with no local slot -- is `*OUT`). The restore
# loop reverted `*OUT` correctly but left `$*OUT` propagating the block's
# leaked value out to the enclosing scope, so `say`/`print` after the block
# kept writing into the block's discarded handle.

plan 3;

class Sink {
    method print(*@_ --> True) { }
}

# Case B exactly as reported: a chained declaration/assignment with no local
# slot for `*OUT` (`$out = my $*OUT = ...`).
{
    my $out;
    my $seen-during;
    {
        $out = my $*OUT = Sink.new;
        $seen-during = $*OUT.^name;
        say "inside (captured, invisible)";
    }
    is $seen-during, 'Sink',
        '$*OUT resolves to the block-local redeclaration during the block';
    isnt $*OUT.^name, 'Sink',
        '$*OUT does not leak the block-scoped redeclaration after the block exits';
}

# A plain reassignment of an ALREADY-EXISTING dynamic var (no `my`) is NOT
# block-scoped and must still propagate -- this must keep working alongside
# the fix above.
{
    my $*shared = 1;
    { $*shared = 2 }
    is $*shared, 2, 'a plain (non-`my`) dynamic-var write-through still propagates';
}
