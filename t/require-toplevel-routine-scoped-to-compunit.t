use v6;
use Test;

# A `require`d/`use`d compunit's own package-less top-level routines (its own
# `sub MAIN`, or an ordinary `sub helper`) must not collide with -- or
# silently overwrite -- a same-named routine the loading scope already
# declared. Raku scopes such a routine lexically to its own compilation unit,
# even though mutsu currently installs it at the shared `GLOBAL::<name>`
# registry key both scopes use for a package-less top-level declaration.
#
# See roast/S06-other/main.t ("MAIN in a module did not get executed") and
# todo/deep/vendor-real-test-module.md's 2026-08-29 entry for the bug this
# pins, and the related cross-module-private-sub-redeclaration finding for
# the sibling case (two modules, not caller-vs-module).
#
# This does NOT pin the mirror-image "a module's own non-exported top-level
# routine must not leak into the loading scope" behavior -- that is a
# separate, still-open gap tracked in
# todo/deep/module-toplevel-private-sub-leak-cleanup.md.

plan 4;

my $exe = $*EXECUTABLE;

# `require`-ing a module whose own top-level `sub MAIN` collides with the
# caller's own top-level `sub MAIN` must not raise X::Redeclaration, and the
# module's MAIN (neither the nested-package one nor the top-level one) must
# ever be auto-dispatched -- the caller's own MAIN wins.
{
    my $r = run(
        $exe, '-I', 't/lib', '-e',
        'sub MAIN($a, $b, *@c) { say "main called $a $b @c[]" }; require ToplevelMainCollision; say "lived";',
        'a', 'b', 'c', :out, :err,
    );
    is $r.out.slurp(:close), "lived\nmain called a b c\n",
        'require-ing a module with a colliding top-level MAIN keeps the caller\'s own MAIN';
    is $r.err.slurp(:close), '', 'no error output';
}

# Same, via `use` instead of `require`.
{
    my $r = run(
        $exe, '-I', 't/lib', '-e',
        'sub MAIN($a, $b, *@c) { say "main called $a $b @c[]" }; use ToplevelMainCollision; say "lived";',
        'a', 'b', 'c', :out, :err,
    );
    is $r.out.slurp(:close), "lived\nmain called a b c\n",
        'use-ing a module with a colliding top-level MAIN keeps the caller\'s own MAIN';
}

# A plain (non-MAIN) top-level routine collides the same way: requiring a
# module with its own private top-level `sub helper` must not clobber the
# caller's own same-named routine.
{
    my $r = run(
        $exe, '-I', 't/lib', '-e',
        'sub helper() { "caller helper" }; require ToplevelHelperCollision; say helper();',
        :out, :err,
    );
    is $r.out.slurp(:close).trim, 'caller helper',
        'require-ing a module with a colliding top-level sub keeps the caller\'s own binding';
}
