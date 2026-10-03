# Declares `trait_mod:<is>` candidates of its own and then `use`s
# TraitScopeInner, the shape of upstream `NativeCall.rakumod` over
# `NativeCall::Types` (#11310). The candidates are hoisted, so they exist
# before the `use` runs; they must still be invisible to TraitScopeInner.
use TraitScopeInner;

unit module TraitScopeOuter;

multi trait_mod:<is>(Routine $r, :$outer-mark!) is export {
    $r.wrap(-> | { "outer:$outer-mark" });
}

# Would capture TraitScopeInner's `is inner-mark` if it leaked there.
multi trait_mod:<is>(Mu:U $type, :$inner-mark!) {
    $type.^add_method('mark', my method mark { "leaked:$inner-mark" });
}

sub marked() is outer-mark(1) { }
our sub call-marked() { marked() }

# The script reaches TraitScopeInner only through this module.
our sub inner-mark() { TraitScopeInner::Marked.mark }
our sub inner-array-type() { TraitScopeInner::Ints.^array_type }
our sub kind-of($x) { pick-kind($x) }
