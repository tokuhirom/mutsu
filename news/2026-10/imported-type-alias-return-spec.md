# An imported lowercase type alias works as a `-->` return type

`sub h(--> word) { 42 }` returned `Nil` when `word` was a lowercase type alias
imported from a module (`my constant word is export = M::T::word`): the
compiler classified every lowercase `--> name` it could not see as a type as a
*definite return value* and sank the body. For an imported `constant` the
compiler now leaves the decision to the runtime, which can tell a type alias
from a value, so the named sub returns `42` like the anonymous one and like
rakudo (#11706). A local constant keeps the compile-time answer, because it is
not yet initialized when a hoisted sub registers.
