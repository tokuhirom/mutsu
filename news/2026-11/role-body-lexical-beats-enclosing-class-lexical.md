# A mixed-in role's body lexical wins over the enclosing class's same-named lexical

`class C { my \iv = 1; my role R[\n] { my \iv = n * 2; method v { iv } } }`:
`C.new but R[5]` and `C.^mixin(R[5])` answered the class's `iv` (1) from the
role's method, because method entry injected the enclosing class's statics over
the role's composition bindings. The bindings are re-applied after the statics.
Interval's `Interval[60].succ` steps by 60 seconds again; its two test files
pass. Closes #17023.
