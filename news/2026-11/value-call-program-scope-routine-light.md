# A named sub called through its code object binds like a direct call

Calling a named sub through its code object (`@list.first(&f)`, `.map(&f)`,
`.grep(&f)`, `sort(&f, …)`, `my &g = &f; g(…)`) went through the general
closure path on every call: the full binder with complete type matching, the
return-type enforcement, an env clone and a full call frame. It cost twice as
much as wrapping the same call in a block, `{ f($_) }`, which takes the
positional light path.

A routine declared at program scope resolves its names through its static
link to that scope (ADR-12529), wherever it is called from. So a value call of
such a routine now binds on the positional light path whenever its signature
and the arguments admit it, exactly as the direct call does (#18083). A routine
nested in another routine, or declared inside a block, still runs through its
code object's captured env.
Named arguments, `is rw`, `state` and the other shapes the light path does not
take keep the general path.

`@chars.first(&unsafe)` over 5000 one-character strings goes from 31,343 to
13,248 instructions per element, now below the wrapped
`@chars.first({ unsafe($_) })` at 15,538.
