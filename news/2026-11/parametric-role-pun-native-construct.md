# The pun of a parametric role is built by the native constructor

`.new` on the pun of a parametric role (`R[Int].^pun.new(:value(5))`, the
class behind every Monad `Maybe[T]` / `Either[L, R]` value) always went
through the full constructor: the class name contains `[`, and the class
carries its role's type arguments, and either fact alone kept it off the
native default constructor. The full path costs about 113,000 instructions
per construction against about 13,000 for a plain class.

When every attribute initializer of such a class is a literal (or the type
seed a typed declaration implies), the type arguments decide nothing but the
attributes' declared types, so the native constructor now serves it, with
those types resolved in its per-class plan. A class whose initializers are
expressions, or that declares BUILD/TWEAK, keeps the full constructor, which
installs the role's bindings for them.

`R[Int].^pun.new(:value(5))` went from 113,000 to 12,500 instructions, and
the same pun mixed with a role (Monad's `pun but Some`) from 104,000 to 49,000
(release build). Monad's `t/14_concurrent_types.t` reduced to one thread and
1,500 chains went from 15.7 s to 13.5 s.
