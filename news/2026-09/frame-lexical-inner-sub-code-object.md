# A `my sub` used as `&name` stays a frame lexical (ADR-0113 slice 3)

The first two slices of ADR-0113 (#9105, #9113) stopped installing a `my sub`
into the program-global routine registry on every call of its enclosing
routine, but only when the body did nothing with it except call it by bare
name. Reading it as a code object (`&name`: passed to `map` or another routine,
stored in a variable, returned) still sent the whole declaration back to the
registry path: a registry install and restore per call, `fn_resolve_gen` moved
twice, and the enclosing routine barred from the light call paths.

Such a routine is now a frame lexical too. The compiler's proof accepts a bare
`&name` read (`CodeVar` in the AST, `GetCodeVar` in the bytecode) as a use that
the chunk's `lexical_routines` table serves. At run time `GetCodeVar` builds the
code object from the definition the declaration derived, with the same builder
a registered routine's `&name` uses, and captures the environment at the point
of use, so an escaping `&name` keeps its captures. Each execution of such a
declaration mints the routine's callable identity, as registration did: every
`&name` read in one activation is the same object and each activation gets a new
one. While a wrapper installed through `&name.wrap` is active, a bare call steps
aside to the ordinary dispatch, which runs the wrapper.

The same slice stops a scalar variable or parameter that shares the routine's
name from disqualifying it (`$name` is `name` in the AST, but it is a different
symbol than `&name`). That was what kept JSON::Fast's `sub EXPORT` helpers
`from-json-changed` / `to-json-changed` on the registry path; both are frame
lexicals now.

Pinned by `t/vm/scope/frame-lexical-inner-sub-code-object.t`: captures,
identity per activation, escaping values, same-name shadowing, `map`/`sort`/`xx`,
recursion through a stored `&name`, `.wrap`/`.unwrap`, `.assuming`, and signature
checks, all checked against Rakudo.

Refs #9103.
