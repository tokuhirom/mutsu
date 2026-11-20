# `with EXPR -> \d` binds each run's container again

#18315 moved a `with`'s once-evaluated condition into a compiler
temporary's raw local slot. `with EXPR -> \d { ... }` desugars to
`my \d = $__with_tmp_N`. When the condition held a container, such as a Hash
or an Array, that declaration still carried the scalar-sharing mark (slice 2
of `docs/scalar-array-sharing.md`). The mark promoted the temporary *by name*
into a shared env cell, which the temporary's later raw stores never
updated. Every later run of the `with` therefore bound the first run's
container:

```raku
for {p => {e => 1}}, {p => {e => 2}} { with .<p> -> \d { say d<e> } }   # 1, 1
```

This broke CSS::Properties. Its `!build-declarations` reads each declaration
through nested `with .<property> -> \decl { with decl<expr> -> \expr`, so every
font descriptor after the first lost its `src`, and 14 test files died with
"No such method 'ends-with' for invocant of type 'Any'".

The temporary already holds the container itself, unitemized, so the plain
store binds the same object. The share mark is no longer emitted for a
`with` temporary.
