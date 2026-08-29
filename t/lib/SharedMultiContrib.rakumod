# Package-less (no `unit module`) on purpose: this contributes a second
# candidate to a multi another, already-loaded module (SharedMultiHost) also
# exports under the same bare name -- the shape
# roast/packages/Advent/lib/Advent/MetaBoundaryAspect.rakumod uses for its
# own `multi trait_mod:<is>`.
multi sub shared-multi(Str $x) is export { "str:$x" }
