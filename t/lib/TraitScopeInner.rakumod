# The module TraitScopeOuter `use`s. Its traits must dispatch to its own
# `trait_mod:<is>` candidate and to the core ones only, never to a candidate
# the loading compunit declared (#11310).
unit module TraitScopeInner;

# Rakudo applies a trait at compile time, so it leaves its mark on the type
# rather than in a variable the mainline would (re)initialize.
multi trait_mod:<is>(Mu:U $type, :$inner-mark!) {
    $type.^add_method('mark', my method mark { "inner:$inner-mark" });
}

our class Marked is inner-mark(1) { }

# A core trait: recorded, never dispatched to a user candidate.
our class Ints is repr('CArray') is array_type(int32) { }

# `is export` on a `my proto` / `my multi` family.
my proto sub pick-kind(|) is export {*}
my multi sub pick-kind(Int) { 'Int' }
my multi sub pick-kind(Mu)  { 'Mu' }
