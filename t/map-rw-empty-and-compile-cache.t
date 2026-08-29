use Test;

# Pins for the two `eval_map_over_items_rw` fixes:
#
# 1. The rw map loop now shares `compile_loop_block_cached` with the List
#    sibling, `first` and the grep loop, instead of running the whole compiler
#    on the block AST at EVERY `.map` call. The cache is keyed on the block's
#    origin `CompiledCode` plus whether a routine is on the stack, so `return`
#    semantics inside the block must stay unchanged.
# 2. `.map` over an EMPTY array returns immediately, before the block compile,
#    the env save/restore and the nested-register frame. Nothing observable may
#    change, including write-back and an enclosing map's own topic.

plan 17;

# --- Empty input -------------------------------------------------------------
my @empty;
my @out = @empty.map(* + 1);
is @out.elems, 0, 'map over an empty array yields no elements';

@empty = @empty.map(*.flat);
is @empty.elems, 0, 'self-assigning an empty rw map keeps the array empty';

# The TWEAK shape that made this hot: an empty `@!` attribute mapped in place.
class Dist {
    has @.resources;
    submethod TWEAK(:@!resources) { @!resources = @!resources.map(*.flat); }
}
is Dist.new.resources.elems, 0, 'empty attribute array maps to empty';
is Dist.new(resources => [1, 2]).resources.elems, 2,
    'non-empty attribute array still maps';

# An empty inner map must not disturb the enclosing map's element write-back.
my @outer = 1, 2, 3;
my @inner;
my @res = @outer.map({ $_ = $_ * 10; @inner.map(* + 1); $_ });
is @res.join(','), '10,20,30', 'inner empty map does not disturb the outer map result';
is @outer.join(','), '10,20,30', 'inner empty map does not disturb the outer write-back';

# --- Write-back still works --------------------------------------------------
my @nums = 1, 2, 3;
my @doubled = @nums.map({ $_ = $_ * 2; $_ });
is @nums.join(','), '2,4,6', 'rw map writes back into the source array';
is @doubled.join(','), '2,4,6', 'rw map returns the mapped values';

# --- The compile cache must not change block semantics -----------------------
# Same closure literal, called repeatedly: the cached compile is reused.
my @acc;
for ^3 -> $i {
    @acc.push: |(1, 2).map(* + $i);
}
is @acc.join(','), '1,2,2,3,3,4', 'a repeatedly-compiled map block stays correct';

# `state` inside a map block is scoped to the closure instance, not shared
# across two distinct blocks that happen to compile to the same shape.
sub counter(@xs) { @xs.map({ state $n = 0; $n++; $n }) }
is counter([1, 2, 3]).join(','), '1,2,3', 'state in a map block counts up';
is counter([1, 2]).join(','), '1,2', 'a fresh call gets a fresh state cell';

# `return` inside a map block ends the ENCLOSING routine (this is the behaviour
# `compile_loop_block_cached`'s `lexically_in_routine` flag preserves).
sub find-first-even(@xs) {
    @xs.map({ return $_ if $_ %% 2 });
    return -1;
}
is find-first-even([1, 3, 4, 5]), 4, 'return inside a map block returns from the routine';
is find-first-even([1, 3, 5]), -1, 'no match falls through to the routine tail';

# Slips out of a map block still flatten.
my @flat = (1, 2).map({ ($_, $_) .Slip });
is @flat.join(','), '1,1,2,2', 'a Slip result flattens into the map output';

# A multi-arity block over an rw array.
my @pairs = (1, 2, 3, 4);
is @pairs.map(-> $a, $b { $a + $b }).join(','), '3,7', 'multi-arity map block still works';

# `compile_loop_block_cached` also marks the block as lexically nested in a
# routine whenever one is on the stack, which is what decides whether a TYPED
# `my` inside the block is frame-scoped. The rw loop now agrees with the List
# sibling on this; a typed declaration must stay block-local either way.
sub typed-in-block(@xs) { @xs.map({ my Int $t = $_ * 2; $t }) }
is typed-in-block([1, 2, 3]).join(','), '2,4,6', 'typed my inside an rw map block works';
is typed-in-block([4]).join(','), '8', 'typed my in the block is fresh per call';
