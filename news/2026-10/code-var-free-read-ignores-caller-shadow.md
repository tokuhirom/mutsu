# A routine's free `&name` no longer reads its caller's `my &name`

A routine that reads a `&name` it does not declare itself — `my $e = &enc`,
`.subst(/x/, &enc)`, `&enc(...)` or a bare `enc(...)` — now gets the binding
visible where the routine was declared. Before, every one of those reads that
was not answered by the routine's own frame fell through to the flat by-name
environment, where a *caller's* same-named `my &enc` sat on top: lexical
scoping degraded into dynamic scoping.

URI::Template hit it head-on. Its class body declares `my &enc` (a percent
encoder) and `my sub uri-encode` passes `&enc` to `.subst`; a method that
calls `uri-encode` keeps an encoder of its own in a local `my &enc`, so
`uri-encode` handed `.subst` the method's encoder, which then rejected the
`Match` it was given. With this fix all eight of the distribution's test files
pass under mutsu.

The declaration-scoped stores scalars already resolve through ahead of the
environment now answer `&name` too: the mainline/bare-block capture cells
(ADR-0024), a class body's `package_lexicals` statics and a module's file-scope
`unit_lexicals` (which now take its `my &name` declarations as well). A `&name`
slot of the running frame — a `&`-parameter or its own `my &name` — still
shadows them all. The value read (`GetCodeVar`), the call (`CallOnCodeVar`),
the bare-call path and TRIR's generic call site share the one resolver.
