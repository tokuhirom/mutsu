# A `$alias := $var` / `$!x := $var` bind now tracks later source writes through nested sub/closure call chains

Closes `todo/deep/attr-bind-source-write-lost-through-nested-sub-call-chain.md`
(three investigation sessions, 2026-08-18/19). The bug: after binding a free /
outer lexical inside a named sub, closure, or method — directly or through a
multi-frame chain like the real vendored `Test.rakumod`'s
`lives-ok { $obj.bind() }` — a later write to the *source* variable
(`$var = 200`) did not show up when reading the bound alias (`$alias` / `$obj.x`
stayed at the bind-time value). Real `raku` tracks the live source. Under
`MUTSU_REAL_TEST=1` this regressed `t/has-attr-binding.t` test 6.

This session found that the previous sessions' framing ("needs a multi-frame
chain"; "the broken ancestor-frame `saved_locals` patch loop is the root
cause") was incomplete: for the plain named-sub shape even a **direct**
one-level call lost the binding, and the `saved_locals` indexing bug —
while real — was not what dropped the cell. Three independent defects
combined, each confirmed with `rust-gdb` cell-pointer traces and one
env-var-gated `Env::insert` watch (backtrace per insert of the watched key;
removed before commit):

1. **The `SetGlobal` `:=`-bind handler minted a disconnected snapshot cell**
   (`src/vm/vm_exec_dispatch.rs`). The source read that produces the bound
   value derefs the source's cell, so the handler always saw a plain value and
   wrapped it in a *fresh* `ContainerRef` — never reusing the source's existing
   authoritative cell (its env entry, or its ADR-0024
   `unit_lexicals[MAINLINE_UNIT_KEY]` capture cell). Every later `$var = ...`
   went through the source's own cell; the alias stayed bound to the orphan
   snapshot. The `SetLocal` twin already did this reuse (both branches in
   `vm_var_assign_set_local.rs`); the `SetGlobal` copy now mirrors it.
   This alone fixed all named-sub shapes, including through `lives-ok`.

2. **The closure-return caller-env writeback treated a cell promotion as "no
   change" and dropped it** (`src/vm/vm_closure_dispatch.rs`). The
   "unchanged captured binding" skip compared the capture-time snapshot with
   the current env value via `values_identical`, whose fallthrough is `eqv` —
   which derefs a `ContainerRef`. A nested `:=` that promoted the caller's
   plain lexical to a shared cell leaves the cell's *contents* equal to the
   snapshot, so the promotion was classified as unchanged and never written
   back to the caller env. Any later sibling call from the caller's frame
   (e.g. `proclaim` inside `lives-ok` — the ticket's mysterious "any extra
   method call after the `try {}` triggers it" observation) then re-merged the
   stale plain value from an un-promoted env tier over the spliced
   `saved_env` copies, and mainline never saw the cell. The skip now uses
   `container_identity_identical` (the previously Capture-private
   `capture_elem_identical`, renamed and exposed): cells compare by `Gc`
   identity, and a cell can never be identical to a plain value.

3. **The rw-writeback "rejoin overlay to captured cell" loop clobbered a
   legitimate rebind** (`src/vm/vm_closure_dispatch.rs`, the
   `pending_rw_writeback_sources` loop before the frame pop). When the live
   env held a *different* cell than the closure's capture-time cell, the loop
   assumed the overlay merely hid the captured cell behind a deref'd value,
   stored the new cell's contents *through* the old cell, and re-installed the
   old cell into env — severing a `:=` rebind performed by a nested callee.
   (This is what still broke the bare-`{ ... }`-block shape once the real
   `Test.rakumod` was loaded.) A different-cell env value now wins: the rejoin
   only applies to a plain overlay value.

Pinned by `t/bind-source-tracks-through-call-chain.t` (8 subtests: direct sub
call, wrapper-sub chain, try-wrapper chain, sub-side source writes, in-frame
tracking, and the `$!x` attribute bind through a lives-ok-shaped chain), all
raku-verified, passing under both the native Test provider and
`MUTSU_REAL_TEST=1`. `t/has-attr-binding.t` now passes 6/6 under
`MUTSU_REAL_TEST=1`.

Debugging notes for posterity: the decisive technique was comparing NaN-boxed
cell pointers across bind/write/read sites (`rust-gdb` `p/x container`,
`x/gx found`), plus one temporary `MUTSU_DEBUG_ENV_WATCH` hook in
`Env::insert`/`insert_sym` printing kind+bits+backtrace per insert of one
watched key — ten log lines reconstructed the whole loss chain. Also,
`sub wrap(&c) { c() }` reproduces what `lives-ok` was needed for before, with
no Test module at all.

Residual related work is recorded in
`todo/tickets/bind-alias-residuals-reverse-write-and-propagation-dedup.md`:
the reverse-direction write (`$alias = 5` should reach `$var` through the
sub-performed bind; raku does, mutsu does not — pre-existing), and the
still-duplicated (and still `saved_locals`-broken) ancestor-frame propagation
loops in the `SetLocal`/`SetGlobal` bind handlers.
