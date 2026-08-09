# Dynamic variables bound before a `start` leak process-wide via shared_vars seeding

## Affected tests

- `t/http-router-plugin.rakutest` (Cro::HTTP dist) — aborts after `ok 4` (subtest
  "Access to configuration with single level route block"). stderr:
  `Too many messages` from `add-message` (test line 12), raised inside the
  `my $test-outer = route { add-message 'o1'; ... }` block at test line 62.
  raku runs the file to completion (`1..7`, rc=0).

## Repro

`tmp/tapdiag-dynleak2.raku` (verified 2026-08-09, release binary vs raku):

```raku
sub s1() { my $*A := 1; start { 0 } }
await s1();
say (try $*A).raku;          # mutsu: 1     raku: Nil

sub s2() { my $*B := 2; start { 0 }; Nil }
s2();
say (try $*B).raku;          # mutsu: 2     raku: Nil  (no await needed)

sub s4() { my $*D = 4; start { 0 } }
await s4();
say (try $*D).raku;          # mutsu: 4     raku: Nil  (assignment, not binding)

# NO leak when the start body references the dynamic (closure capture owns it):
sub s3() { my $*C := 3; start { $*C } }   # both: read works, no leak after return
# NO leak when start runs BEFORE the binding:
sub s5() { start { 0 }; my $*E := 5; }    # both: Nil
```

The discriminator: the leak happens iff a `start` is spawned **while the dynamic
is in the frame env** and the spawned block does **not** capture that dynamic as
a free variable.

## Root cause

`start` spawns via `spawn_callable_promise` (src/runtime/builtins_system.rs:171)
→ `clone_for_thread_for_block` → `clone_for_thread_excluding`
(src/runtime/runtime_thread.rs:130). Its seeding loop (runtime_thread.rs:164-250)
copies **every** parent env entry into the lineage-shared, bare-name-keyed
`shared_vars` store. The skip list (runtime_thread.rs:179-194) excludes
`_`, `@_`, `%_`, `/`, `!`, `$/`, `$!`, `self`, `__mutsu_*`, `&*`, `?*` — and
exactly ONE dynamic variable, `$*CWD`/`*CWD`, with a comment already stating the
general rule: "in Raku, dynamic variables like $*CWD are thread-local". Every
*other* `*`-twigil key (`*A`, `*CRO-ROUTER-ROUTE-HANDLER`, ...) is seeded into
the store.

The frame then returns; nothing removes the seeded entry. Any later lookup of
`$*A` anywhere in the lineage misses the env and falls back to the store:
src/vm/vm_env_helpers.rs:687-691 (`get_env_with_main_alias_inner`: "Fall back to
shared_vars for cross-thread visibility"). So a dead frame's dynamic is now
visible process-wide — dynamics are supposed to resolve through the *live*
caller chain only (`is_dynamic_var_name`, src/env.rs:105-116 documents this).

Why the "block references the dynamic" case does NOT leak: `block_captured_scalars`
(runtime_thread.rs:13-101) excludes the block's own captured scalars from
seeding, and a referenced dynamic is a free var of the block, so it never enters
the store.

Cro chain: `RouteHandler!invoke-internal`
(Cro/HTTP/Router.rakumod:208-215) does `my $*CRO-ROUTER-ROUTE-HANDLER := self;`
and then `start { ... }` (the handler body). The subtest-4 requests run two
handlers of `$test-inner` (whose `plugin-config` holds `["i1","i2"]`); each spawn
seeds `*CRO-ROUTER-ROUTE-HANDLER` into the store. Back at the top level, the
next `route { add-message 'o1' }` calls `router-plugin-get-innermost-configs`
(Router.rakumod:1503-1513), whose `with $*CRO-ROUTER-ROUTE-HANDLER` now finds
the leaked handler, returns its 2-element config list, and `add-message` dies
"Too many messages". Verified directly: inserting
`note (try $*CRO-ROUTER-ROUTE-HANDLER).raku` before test line 62 prints the full
leaked RouteHandler instance (with `plugin-config ... => ["i1", "i2"]`) under
mutsu; raku would print Nil.

## Fix direction

In the seeding loop of `clone_for_thread_excluding`
(src/runtime/runtime_thread.rs:179-194), generalize the `$*CWD`/`*CWD`
special-case to all dynamic variables:

```rust
|| key.with_str(crate::env::is_dynamic_var_name)   // `*x` / `$*x` env keys
```

(`is_dynamic_var_name` is `pub(crate)` in src/env.rs:116 and already matches
both the bare `*x` and sigiled `$*x` key forms — check its exact contract.)

The spawned thread keeps SEEING parent dynamics: the child env is a clone of the
parent env (same function, below the seeding loop), so read access inside the
worker is unaffected; only the name-lane sharing (and hence the post-return
process-wide visibility) is removed. This matches Raku semantics: dynamics are
per-thread; a `start` snapshots the dynamic context for reads, and rebinding
inside a worker never propagates to the parent.

Defense in depth (optional): also filter `is_dynamic_var_name` keys in
`sync_shared_vars_to_env` (src/runtime/runtime_shared_vars.rs:538-543, next to
the existing `self`/`?` filter) so stale store entries created by older binaries
or other lanes can never be pulled back into a live env.

Risks:

- Code that (incorrectly, per Raku) relied on a worker's dynamic ASSIGNMENT
  being visible to the parent after `await` would regress — that behavior is
  non-Raku, but grep t/ for tests asserting it before landing.
- A dynamic holding a shared container (Channel/Array in `$*FOO`) still shares
  through the container itself; only the name binding stops being shared. That
  is the correct Raku behavior.

## Verification

- `tmp/tapdiag-dynleak2.raku`: all five probes must print exactly what raku
  prints (Nil / Nil / 3+Nil / Nil / Nil).
- `t/http-router-plugin.rakutest` must get past subtest 4 without the
  "Too many messages" abort.
- **The file WILL then reveal a further failure** (measured by probe, 2026-08-09:
  bypassing the leak inside a copied test made the file run 1..7 with exactly one
  failure): subtest 5 "Access to configuration with include", inner test 2
  "Local configuration in included route handler not affected by outer" gets
  `o1,o2` where `i1,i2` is expected — i.e. the *included* handler's
  `get-innermost-plugin-configs` returns the OUTER route block's config. The
  `copy-adding`/`merge-plugin-config`/`self.bless: :$!plugin-config` shape was
  probed standalone and works in mutsu (tmp/tapdiag-bless.raku), so this is a
  separate, not-yet-root-caused bug (suspect: cross-thread shared-store
  interference with the handler instance attributes at request time). Budget it
  as its own diagnosis; do not fold into this fix.
- Roast: S17 concurrency files and `t/` dynamic-variable tests (regression net:
  the seeding loop is hot for all `start`-based code).
