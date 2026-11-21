# A closure sees a code variable its block declares later

In Raku a `my` lexical is in its scope's pad from the scope's entry, so a
closure created before the declaration of `&f` still names that `&f`:

```raku
sub f() { 'outer f' }
sub mk() { my &g = -> { f() }; my &f = -> { 'block f' }; &g }
say mk()();    # block f
```

mutsu captures a closure's free variables when the closure is created, and the
later `my &f` was a fresh binding the capture never saw. The call found `&f` only
by walking the closure frame's callers, which is the dynamic-scope leak
ADR-12529 retires. So a closure returned out of its routine called the outer
`sub f` (or a caller's own `&f`), and a recursive
`my &fact = -> $n { ... fact($n - 1) }` returned from its routine died with
`Unknown function: fact`.

A post-parse pass (`parser::forward_code_vars`) now gives such a declaration the
shape mutsu already captured correctly. It applies to a plain `my &f = init` that
a closure in an earlier statement of the same list names, or that its own
initializer names. The pass declares `my &f` at the start of the statement list
and compiles the declaration statement as `&f = init`.

This is #18248's first slice: with the closure frame's caller walk switched off,
the files that still need it went from seven to three. The remaining shapes are
a closure inside an `@s` declaration's own initializer and two routine-call
shapes. `t/routines/closure/closure-sees-later-code-var-declaration.t` pins the
behaviour against rakudo.
