# `my @p := @!p` binds the running invocant's attribute, not the caller's

Since `@!x` / `%!x` reads go through an attribute slot (ADR-18356 D2,
#18394), the slot is no longer a by-name local of the method frame, and a
`my @p := @!p` bind looked for an existing source cell by name in the env
chain. A callee's env is a scoped child of its caller's, so when the caller
was a method of the same type that had bound `my @p := @!p` itself, the
callee joined the *caller's* attribute: `$a.run({ $b.add(1) })` pushed into
`$a`. The Commands distribution's `extended-help-from-hash`, called from a
command handler, built an empty help object and filled the outer one
(`t/01-basic.rakutest` fell from parity to 33/38).

A bind source that is the frame's own aggregate attribute slot is now the
invocant's attribute, which the source read has just refreshed from `self`:
the bind never probes the env chain for it and never splices it into a
caller's frame (#18432).
