# `⚛+=` and `⚛-=` accept an array or hash element

`@a[0] ⚛+= 5` and `%h<k> ⚛-= 1` were a parse error: the compound-assignment parse sites only took a
variable name on their left, although `@a[0]⚛++` and `atomic-add-fetch(@a[0], 5)` already worked on an
element ([#11812](https://github.com/tokuhirom/mutsu/issues/11812)).

An element target is now parsed in the expression-level assignment layer (so it works as a statement,
inside parentheses and as an operand) and becomes the call of Rakudo's own `infix:<⚛+=>` /
`infix:<⚛-=>`, the same shape a variable target already had. `try_compile_atomic_elem_call` maps both
onto the element's one atomic add, negating the delta for the subtract form, so the update is atomic
and a non-native target (`my @plain`, `%h`) is refused as `infix:<⚛+=>(Int:D, Int:D)`, as in Rakudo
([#11834](https://github.com/tokuhirom/mutsu/issues/11834)). `atomic_compound_call` now takes the target
expression instead of a variable name, so the three variable-form parse sites share it unchanged.

Test: `t/concurrency/thread-lock/atomic-compound-assign-element.t`. Closes
[#12005](https://github.com/tokuhirom/mutsu/issues/12005).
