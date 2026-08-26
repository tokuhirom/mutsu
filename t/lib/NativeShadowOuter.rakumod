unit module NativeShadowOuter;

# `shadow-before` is declared BEFORE `need NativeShadowInner` runs, to prove
# declaration order relative to the `need` does not matter -- Raku's own hoist
# pass makes every top-level sub in a compunit visible from the start of that
# compunit regardless of its textual position.
our sub shadow-before($a) is export { "local-before($a)" }

need NativeShadowInner;

our sub shadow-our($a) is export { "local-our($a)" }
my sub shadow-my($a) { "local-my($a)" }
multi sub shadow-multi($a) is export { "local-multi($a)" }
sub shadow-noexport($a) { "local-noexport($a)" }

# Each of these calls its same-named counterpart above by its bare name, from
# within NativeShadowOuter's own lexical scope -- exactly like
# `Compress::Zlib.pm6`'s `compress` wrapper calling `_internal-compression`,
# which in turn calls the bare, unqualified names re-exported by `need
# Compress::Zlib::Raw`. Every one of these must resolve to the local wrapper
# declared just above, never to NativeShadowInner's same-named native sub.
sub call-our() is export { shadow-our(1) }
sub call-my() is export { shadow-my(1) }
sub call-multi() is export { shadow-multi(1) }
sub call-before() is export { shadow-before(1) }
sub call-noexport() is export { shadow-noexport(1) }
