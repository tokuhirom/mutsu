# Declaring a class no longer env-syncs every local

The #10960 fix bounded the `needs_env_sync` fold for a named `sub`, but a
`class` declaration still marked **every** local of its frame env-synced, so a
top-level loop paid an env mirror write on each store as soon as the file
declared a class — the cost the `method_call`, `attr_read` and `new` rows of
`benchmarks/micro/primitive-ops.py` carried (#10999).

`src/compiler/lazy_body_env_sync.rs` now bounds a class plan too. Its
registration reads outer lexicals by name through several channels, and each
is enumerated: the compiled method bodies (nested closures and subs
included), their signatures' declaration-time expressions (defaults, `where`
constraints, trait and shape arguments, nested signatures), the attribute
descriptors' default/`where`/`is default` chunks, the trait and
`is Parent[Args]` chunks, the class-body statement chunks run at registration,
and the type names the header, attributes and signatures mention. A computed
class or method name, anything still evaluated from raw AST at registration, a
`token`/`rule` in the body, or a body that reads names no op scan can bound
keeps the old every-local fold.

The fix also corrects a latent index mix-up: the bounded set held
`decl_plans` indices (the `RegisterDecl` operand) but `compute_needs_env_sync`
looked it up with the `sub_decl_plans` index, so once non-sub declarations
could be bounded an unbounded sub could have been mistaken for a bounded one.
The set is now `bounded_lazy_decl_plans` and is keyed by the declaration index
everywhere; a nested declaration inside a body counts as bounded only when it
is a sub, since a nested class's body-statement reads are not folded into the
enclosing body's free variables.

On the issue's repro (100,000 iterations of `$t = C.m; $i = $i + 1` after
`class C { method m() { 1 } }`, callgrind) `exec_set_local_op`'s inclusive Ir
is now identical to the same loop with the class removed.
