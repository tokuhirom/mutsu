# A `*%options` slurpy parameter read inside a nested `start {}` block returns the FIRST call's value on every later, purely-sequential call

## Affected tests

- `t/composer.rakutest` (Cro::Core) test 134 "Correct message, implying correct options passing
  and correct transforms" (133/134 pass) — root cause of the `composer.rakutest` row in
  `todo/tickets/cro-core-composer-and-conditional-connector-failures.md`.
- `t/connection-conditional.rakutest` (Cro::Core) test 23 "Conditional correctly evaluated to
  default" (22/23 pass) — root cause of that ticket's other row.

Both hit the exact same shape: a `Cro::Connector`'s `method connect(*%options) { start
Transform.new(prepend => %options<prepend>) }` (or `:cond(%options<cond>)`) is called twice in
sequence with *different* option values via two separate `.establish(...)` calls. The second call
reads back the FIRST call's option value instead of its own.

This ticket **replaces** the "Next step: repro each subtest in isolation" instruction in
`cro-core-composer-and-conditional-connector-failures.md` — both rows there are this one bug.

## Repro (verified, no Cro)

`tmp/repro-slurpy-start-sub.raku`:

```raku
sub connect(*%options) {
    start %options<prepend>
}

my $h1 = await connect(prepend => 'un');
say "h1: ", $h1;
my $h2 = await connect(prepend => 'in');
say "h2: ", $h2;
```

- raku: `h1: un` / `h2: in`.
- mutsu (debug and release): `h1: un` / `h2: un`.

Note this is **not a concurrency bug** — the two calls are fully sequential (`await`ed one after
the other, no overlap), on the same (main) thread. Negative controls, both print correctly under
mutsu (`tmp/repro-connect-slurpy-options-nostart.raku`, `tmp/repro-connect-named-param-start.raku`):

- Without the nested `start` (`Holder.new(prepend => %options<prepend>)` called directly, no
  `start`): correct on both calls.
- With `start` but a **regular** named parameter instead of a slurpy hash (`method connect(:$prepend) { start Holder.new(:$prepend) }`): correct on both calls.

So the trigger is specifically **slurpy `%`/`@` parameter + a nested `start` block reading it**.

## Root cause (narrowed, not fully pinned down)

`%options` (or any `*%hashname`/`*@arrname` slurpy parameter) is a bare name starting with `%`/`@`
whose second character is an identifier start, so `is_plain_lexical_name`
(`src/vm/vm_call_method_mut_ops.rs:2204`) — a purely syntactic check, no signature/slurpy
awareness — treats it exactly like a genuinely-shared `my %h` lexical for the purposes of the
name-keyed `__mutsu_atomic_hash::`/`__mutsu_atomic_arr::` shared-store lane (confirmed via
`rust-gdb`: `SharedStore::get` is hit with key `"__mutsu_atomic_hash::%options"` when the `start`
block's body reads `%options<prepend>`).

The read gate that is supposed to prevent this — `container_name_is_redeclared(name)`, checked
immediately before the atomic-hash-key lookup in `vm_var_assign_local_get.rs:130-146` (and its
`vm_env_helpers.rs:652-665` twin) — is exactly the mechanism `mask_thread_redeclared_params`
(`src/runtime/runtime_shared_vars.rs:252`) exists to make return `true` for a **slurpy** `@`/`%`
parameter's name for the duration of a call (see `news/2026-08/hash-slurpy-param-thread-mask.md`,
which fixed the analogous *cross-thread* leak of the same name). That fix's own repro attempts
"remained elusive" per its own writeup — this ticket's repro is the first one that pins it down
concretely, but for the **sequential same-thread** case, not the concurrent one that fix targeted.

Two live hypotheses for why the mask does not prevent the stale read here (not yet distinguished —
next step is to gdb-trace `container_name_is_redeclared("%options")`'s actual return value and
`self.thread_redeclared_vars`'s contents at the moment the SECOND call's `start` block body runs):

1. **Mask lifetime mismatch across the `start` boundary.** `mask_thread_redeclared_params` brackets
   the *synchronous* portion of `connect`'s call (mask on entry, `unmask_thread_redeclared_params`
   at `vm_call_named_inner.rs:468`, presumably at/near the call's own return) — but the `start`
   block's body itself runs on a **cloned Interpreter** (`clone_for_thread`) on another thread,
   *after* `connect` has already returned the `Promise`/started-Sub value. If the mask is
   unconditionally lifted when `connect` returns (immediately, since `start` doesn't block), the
   clone spawned for the `start` may run its body — and thus read `%options` — **after** the parent
   frame has already unmasked, so the read gate sees an unmasked name and falls through to the
   stale atomic entry regardless of which call it belongs to.
2. **The atomic entry from call 1 is never cleared, and the mask (even if correctly active for call
   2) only stops call 2's OWN `%options` reference from being *misread as shared* — it does not
   stop something ELSE (e.g. the first call's now-orphaned atomic-hash entry) from still being
   *visible* if some other check in the read path does not consult
   `container_name_is_redeclared` consistently. Audit both read sites
   (`vm_var_assign_local_get.rs:140-146` and `vm_env_helpers.rs:652-665`) plus whatever WRITE path
   originally created `__mutsu_atomic_hash::%options` in the first place (grep
   `__mutsu_atomic_hash::` write sites in `builtins_atomic_shared.rs`/`runtime_shared_vars.rs`) to
   see which one actually ran for a *slurpy parameter* despite the mask.

## Fix direction

Do NOT touch `is_plain_lexical_name` itself — it is reused by many unrelated call sites (variable
declarations, plain lexicals) where the syntactic check is exactly right; only a *parameter*
binding context needs slurpy-awareness, and that context is only known at the specific call sites
above.

Most promising angle: make the mask's lifetime span the **spawned thread's entire body**, not just
the synchronous portion of the call that established it — i.e., a slurpy parameter's masked name
should propagate into `clone_for_thread`'s child lineage (similar to how `thread_param_shadow_vars`
already threads through `clone_for_thread_excluding`, per `mask_thread_redeclared_params`'s own
doc comment) so a nested `start`'s clone inherits "this name is a fresh per-call parameter, not a
shared lexical" for as long as ITS OWN body runs, independent of whether the synchronous parent
frame has already returned and unmasked.

Verify by first adding `MUTSU_TRACE`-style instrumentation (or a `rust-gdb` breakpoint per
CLAUDE.md's debugging guidelines) at `container_name_is_redeclared`'s call in
`vm_var_assign_local_get.rs:130` for the second `connect` call, to confirm hypothesis 1 vs 2 before
writing the fix.

## Verification

- `tmp/repro-slurpy-start-sub.raku` prints `h1: un` / `h2: in`.
- `t/hash-slurpy-param-thread-mask.t` (the existing concurrent-leak pin) stays green — this fix
  must not regress it.
- `t/composer.rakutest` / `t/connection-conditional.rakutest` (Cro::Core dist tests, via
  `bash tmp/cro-suite-run.sh core`) both reach 134/134 and 23/23.
- `make test` + roast S17 whitelist locally, full roast via CI.
