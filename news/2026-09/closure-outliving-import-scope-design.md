# A closure outliving its import scope gets an ADR, and the issue's own theory doesn't survive tracing

Issue [#8751](https://github.com/tokuhirom/mutsu/issues/8751) reported that a closure created
inside `sub EXPORT`'s own `use`-containing body loses the routine it imported once `sub EXPORT`
has returned and the import scope has popped, even when the closure is invoked later.

The issue's own hypothesis — a `unit module`'s exported sub registering only under the ambiguous
`GLOBAL::name` key, indistinguishable from an import alias — does not survive tracing the actual
registry keys through `pop_import_scope`. The module's own definition (`Inner::real-greet`)
survives the pop exactly as designed; the only key removed is the ephemeral alias `import_module`
installs for that one `use` statement, which is correctly stripped by the same mechanism
`roast/S11-modules/lexical.t` requires. The bug is that the closure has no other route back to the
routine once that alias is gone — a different manifestation of the "no real lexical scope, just a
dynamic flat-registry stack" gap already named for [#7612](https://github.com/tokuhirom/mutsu/issues/7612)
and [ADR-0081](../../docs/adr/0081-compunit-scoped-module-import-aliases.md). The issue's noted
"bare-file module works" case is traced too, and shown to be an accidental side effect (a bare-file
module's own definition happens to be recorded permanently at load time under the same key an
import would use) rather than evidence of a working fix.

[ADR-0108](../../docs/adr/0108-closure-must-pin-its-defining-blocks-routine-imports.md) proposes
giving a closure a private, immutable capture of the routine aliases live in its defining block at
creation time — the same lexical-lifetime guarantee ADR-0092's env-tier chain already gives
captured variables, extended to cover the routine registry — with an alternative design that rides
the existing env-tier machinery instead of adding a second capture channel. This is the design
handoff for the follow-up implementation; the issue remains open until one of the two mechanisms
ships.
