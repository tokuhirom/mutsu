# ADR-0019 Phase F: .^lookup Method accessors fixed, cache-invalidation dedup

Two small ADR-0019 Phase F slices landed.

`.^lookup`/`.^find_method` return a `Sub`-shaped callable rather than the `Method` `Instance`
object `.^methods` builds (`todo/tickets/classhow-lookup-returns-sub-not-method-instance.md`, found
while scoping Phase F box F1/F2). Calling a `Method`-only accessor like `.is_dispatcher` or `.multi`
on that result used to fall into the callable-compose fallback and silently return a bogus
`<composed-method:NAME>` callable instead of a real answer. `methods_instance_ops.rs`'s dispatch
fallback now answers both accessors directly, matching `raku` ground truth: a non-multi method or
submethod answers both `False`; a multi method's dispatcher-shaped value answers `is_dispatcher`
`True` but `multi` falsy; each individual `.candidates[N]` entry answers `is_dispatcher` `False` but
`multi` `True`. Pinned by `t/classhow-lookup-method-is-dispatcher-multi.t`, verified byte-for-byte
identical against `raku`. The deeper representation mismatch (Sub vs. Method Instance) stays open
for a future F1/F2 design pass.

Separately, Phase F box F5 ("remove superseded method caches and manual invalidation") calls out a
"trivial first PR": the same method/function-resolution cache-clear block was duplicated verbatim
at 7 sites across 4 files (module `use`/`import`/`no`/`need`, a `my sub` leaving block scope, a
fresh sub installation, and class/role/enum registration). Extracted into one shared
`Interpreter::invalidate_method_dispatch_caches()`. This also fixed a latent inconsistency where 3
of those 7 sites forgot to clear `resolved_seq_cache` while the others did — now every site clears
the same full set. The eager clears stay in place rather than migrating to the generation-based
lazy scheme `refresh_method_caches_for_generation` already uses for method-only caches, because
`func_multi_resolve_cache`/`func_multi_type_cacheable` (plain multi *sub* dispatch) have no
generation guard at their read site yet — a separate, deeper F5 slice.
