# A role method skips installing type bindings it cannot observe

Every call of a role method installed the role's composition bindings (`T` of
`role R[::T]`, plus the per-composition renames of nested generic classes) into
a fresh env overlay before binding its parameters, and kept them out of the
exit merge afterwards. Most role methods never name them (`method is-some {
self ~~ Some }`), so the work was pure overhead: ~1.9k of a ~15.5k-instruction
trivial role method call.

The fast method binder now skips the install when none of the binding names
appears among the body's constant name tokens or the signature's type names,
and nothing can reach them by a name known only at run time (closures,
`EVAL`, symbolic lookups, `CALLER::`-style readers anywhere in the program, a
def with its own captured env). Both token sets are computed once per chunk /
method definition.

Monad's `t/14_concurrent_types.t` chain iteration (callgrind slope, profiling
build): 7.57M -> 6.70M instructions (-11.5%); role binding installs per
iteration 116 -> 45 (#17331).
