# A block called directly reads the routine's `@_`

A block has no `@_` of its own unless its signature declares one. Inside
`sub f(*@_) { { @_.elems }() }`, the block reads the routine's `@_`. In mutsu
it read the argument list of its own call instead, which every call writes
under that name: `f(1, 2, 3)` answered `0` (#13990). A block passed to `.map`
was already right, because that call path leaves `@_` alone.

The compiler now compiles a block's read of an `@_` it does not declare as an
outer access (`emit_outers_var_access`), when an enclosing routine declares
`@_` itself. An `@_` the parser gives a block implicitly (the block reads it)
does not count. It belongs to the innermost block that reads it (roast
`S04-statements/if.t`'s `if 1, 2 { @_ }`), so a nested block still gets its
own there. A block two levels down still reads the empty `@_` of the middle
block's call (#14024).
