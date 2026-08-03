# A `my $tap` in a loop body reverts to the previous iteration's Tap once a connection is accepted

```raku
my $port = 31427;
for ^3 -> $round {
    my $tap = IO::Socket::Async.listen('localhost', $port).tap(-> $c { $c.close; });
    my $c = await IO::Socket::Async.connect('localhost', $port);   # <- the trigger
    $c.close;
    note "round $round tap=", $tap.WHICH;
    $tap.close;
}
```

```
round 0 tap=Tap|46
round 1 tap=Tap|46      <- round 0's Tap object, not the one just created
round 2 tap=Tap|77
```

Without the `connect` (`tmp/cap10.p6`) every round gets its own Tap
(`46 / 88 / 130`), including with an unrelated `await start { 1 }` in the body.
It is accepting a **connection** that does it.

`gdb` on `native_tap` shows the same `attributes` pointer for round 0 and round 1
(`attributes=0x7fffe00adb50` both times), and `register_async_listener` /
`set_listener_closed` disagree: round 1 registers listener **2** and then closes
listener **1**. So round 1's listener is never closed and stays bound forever.

## Why it matters

This is the whole "a stopped Cro server keeps serving" family, and it is what
still blocks the vendored Cro::HTTP multi-server tests after
`news/2026-08/supply-tap-close-cascades-upstream.md` made `Cro::Service.stop`
close its pipeline properly:

- `tmp/mw6.p6` (six sequential `Cro::HTTP::Server`s on one port) serves two
  rounds and then returns empty bodies;
- `t/http-middleware.rakutest` passes its first subtest 4/4 and then hangs;
- `http-auth-basic`, `http-session-*` and `router-auth` are the same shape.

It is almost certainly the "secondary anomaly" recorded with
`news/2026-08/threaded-array-mutation-escapes-to-the-caller.md`: a
`for 1..3 -> $i { say "round $i"; …Cro request…; say "round $i status" }` printed
`round 2 status` in *every* iteration. Same shape — a loop-body scalar reverting
to another iteration's value once a request has run. `tmp/mw6.p6` reproduces that
one directly (it prints `round 2:` six times).

## Where to look

The accepted connection runs the tap callback on a spawned worker
(`spawn_gc_helper_thread` / `spawn_user_thread` in
`src/runtime/native_methods/socket_async.rs`), which arms the cross-thread shared
lane. The caller's `my $tap` is a plain scalar, so the suspect is the stale
snapshot path — `sync_shared_vars_to_env` / `pending_caller_var_writeback`
pulling the previous iteration's value back over the live binding, the same
mechanism `thread_redeclared_vars` masks for re-declared names
(`src/runtime/runtime_shared_vars.rs`). Note the loop body's `my $tap` *is* a
re-declaration each iteration, so either the mask is not being set here (the
compiler skips `SetVarDynamic` for a `my` it considers already declared in the
scope — see `is_default_init` / `already_declared` in `src/compiler/stmt.rs`) or
it is being cleared by the spawn before the next round.

Two facts are already established with `rust-gdb -batch`, so don't re-derive
them:

- **The mask *is* being set.** Breaking on the `thread_redeclared_vars.insert`
  in `exec_set_var_dynamic_op` prints `c`, `tap`, `c`: round 0's `my $tap` runs
  before any thread has armed the lane (so it is correctly not masked — there is
  nothing to shadow yet) and round 1's `my $tap` *is* masked. So the write side
  (`set_shared_var_sym`) and the bulk read side (`sync_shared_vars_to_env`) are
  both already gated for this name.
- **The stale value therefore arrives some other way.** `$tap.close` takes its
  invocant off the stack from a `GetLocal`, and `exec_get_local_op` prefers
  `self.locals[idx]` for a non-Nil scalar — so the suspect is a writeback into
  the *slot* (or into `env` on a path that bypasses the slot) which skips the
  mask. `sync_shared_vars_for_names` (`src/runtime/runtime_shared_vars.rs`) is
  one such path: unlike `sync_shared_vars_to_env` it does **not** consult
  `thread_redeclared_vars` at all. `pending_caller_var_writeback` /
  `apply_pending_rw_writeback`, which drain a synced name straight into the
  caller's slot, are the other two candidates. Check those three first.

## Related, already fixed here

`IO::Socket::Async.listen` used to `TcpListener::bind(&str)`, which walks *every*
address the host resolves to and takes the first that succeeds. `localhost`
resolves to both `[::1]` and `127.0.0.1`, so a re-listen whose previous listener
was still bound to the first address quietly bound the *other* one: two listeners
for one `localhost:port` coexisted (visible as two LISTEN rows in `ss -tan`), a
client reached whichever its own resolution picked, and only a third round hit
EADDRINUSE. `listen` now resolves once and binds that single address, so a
genuinely-busy port is a real error the existing retry loop waits out — which is
what made the stale-`$tap` bug above visible at round 2 instead of round 3.

Pin when the `$tap` bug is fixed: a `t/io-socket-async-relisten-loop.t` running
the loop above. Never hardcode a port — allocate one from a port-0 listen and
reuse that number (per the `t/io-socket-recv-limit.t` lesson).
