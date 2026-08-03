# A spawn inside an initializer no longer reverts the variable being declared

A `my` whose *initializer* starts a worker thread could end up holding the value
of the binding it was about to shadow. In a loop that meant every iteration's
variable silently reverted to the previous iteration's:

```raku
for ^3 -> $round {
    my $tap = IO::Socket::Async.listen('localhost', $port).tap(-> $c { $c.close });
    my $c = await IO::Socket::Async.connect('localhost', $port);   # the trigger
    $c.close;
    note "round $round tap=", $tap.WHICH;   # 46 / 46 / 77
    $tap.close;
}
```

Round 1 printed round 0's `Tap`, so `$tap.close` closed the *previous* round's
listener and left the current one bound forever. That is the whole "a stopped
server keeps answering" family: `Cro::Service.stop` closes its pipeline through
exactly this shape, so a restarted `Cro::HTTP::Server` kept serving from the
listener it was supposed to have released.

## Root cause

The cross-thread shared store is keyed by bare name, so it cannot represent two
concurrently-live bindings of one name. `thread_redeclared_vars` masks a name a
frame has re-declared, keeping the fresh binding out of that lane; the mask is
dropped again at the next `clone_for_thread`, which first force-`declare`s the
name's *current* value into the lineage. That handshake rests on a premise —
"the current value is the binding the mask was protecting" — and the premise is
false while a declaration is still **in flight**.

`my $tap = ...listen(...).tap(...)` runs `SetVarDynamic` (declare, mask set),
then evaluates the initializer, then stores. The `.tap` spawns a worker *during*
the initializer, so `clone_for_thread` ran between the declaration and its
store: neither the slot nor `env` held the new binding yet, both still carried
the outer one. The spawn published that outer value into the lineage and then
unmasked the name, and the `await`'s `sync_shared_vars_to_env` dutifully pulled
it back over the slot the initializer had by then filled in.

`rust-gdb -batch` showed the sequence directly, with one breakpoint per
mechanism (`DECL "tap"` → `CLONE_FOR_THREAD retain` → `SYNC_SHARED_PUSH "tap"` →
`CALLERWB_APPLY "tap"`), and round 2 — where no spawn falls in that window —
skipping the last two.

## Fix

`thread_decl_in_flight` records the window between a declaration and the store
that ends it (`SetVarDynamic` inserts, `exec_set_local_op` removes). Within it,
`clone_for_thread` keeps the mask and seeds the name only if absent instead of
force-declaring a value that is about to be stale. The window closes at the
store, so the very next spawn republishes the binding normally and a genuinely
shared variable keeps working; the set is empty for single-threaded programs.

Pinned by `t/io-socket-async-relisten-loop.t` (green on raku too), which
allocates its port from a port-0 listen rather than hardcoding one. All 99
whitelisted S17 concurrency roast files stay green.

## Effect on the vendored Cro::HTTP suite

`t/http-middleware.rakutest` no longer hangs at the end of its first subtest —
it now runs the second subtest's assertions as well. Serving from a *third*
server bound to the same port still returns empty bodies, so the multi-server
files are not green yet; that remaining facet stays tracked in
`todo/tickets/async-listener-not-freed-when-relistening-in-a-loop.md`.
