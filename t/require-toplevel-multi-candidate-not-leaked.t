use v6;
use Test;
use lib 't/lib';

# A package-less top-level `multi sub` contributed by a module must NOT be
# treated as a "leaked" declaration and swept away, even when it shares its
# bare name with a multi another, already-loaded module also exports.
#
# Multi routines are additive across compilation units by design (many
# modules legitimately contribute candidates to the same shared name -- e.g.
# a custom `multi trait_mod:<is>` alongside Test.rakumod's own). Export
# bookkeeping is a per-name set, not a per-candidate one, so "was this name
# already exported by an earlier module" cannot be told apart from "this
# module's own new candidate happens to share that name" by a naive diff --
# which is exactly the bug this pins.
#
# See roast/integration/advent2011-day14.t's `Advent::MetaBoundaryAspect`
# fixture (a package-less `multi trait_mod:<is>(...) is export` that lost its
# own candidate the same way, silently breaking the custom trait it
# registers) and todo/deep/vendor-real-test-module.md's 2026-08-29 entry.

plan 2;

my $exe = $*EXECUTABLE;

my $r = run(
    $exe, '-I', 't/lib', '-e',
    'use SharedMultiHost; use SharedMultiContrib; say shared-multi(1); say shared-multi("a");',
    :out, :err,
);
is $r.out.slurp(:close), "int:1\nstr:a\n",
    'both modules\' candidates for the shared multi are callable after loading';
is $r.err.slurp(:close), '', 'no error output';
