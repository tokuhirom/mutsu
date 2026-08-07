# A `state` reset must reach the cross-thread cell

Raku clones a block every time its enclosing block runs, so a `state`
declared inside a loop body restarts on every execution of the loop
*statement*. mutsu implements that by dropping the variable from the state
store at loop-statement entry (`reset_state_locals_in_range`), which makes the
next `StateVarInit` re-run the initializer.

That drop reached only half the storage. Once any thread has been spawned,
`StateVarInit` stops resolving a `state` through the local `state_vars` map and
resolves it through a shared `ContainerRef` cell in `shared_vars` instead — the
cell that lets concurrent calls to one routine (`await (^3).map: { start f() }`)
observe one live container. `remove_state_var` dropped the local entry and left
the cell in place, so from the first `start` onward the reset was a silent
no-op and a loop-body `state` kept counting up across every later execution of
its statement:

```raku
class TC {
    method m(*@in) { my @r; for @in { @r.push(++state $s) }; @r.join(',') }
}
say TC.m(1, 2, 3);                  # 1,2,3
say await start { TC.m(1, 2, 3) };  # 1,2,3
say TC.m(1, 2, 3);                  # was 4,5,6 — raku says 1,2,3
```

`remove_state_var` now drops the shared cell too, under the same normalized key
`StateVarInit` installs it with.

## Why it surfaced now

The counter this broke in practice is Cro's, in `Cro.compose`:

```raku
for flat @components-in Z @components-in[1..*] -> $comp, $next {
    ++state $split;
    ...
    return Cro.compose: |@components-in[^$split], Cro::ConnectionManager.new(...), ...;
}
```

`$split` is the index the connection manager gets spliced in at. Once
`Cro::Service.start` had spawned a thread, the next `compose` began with the
previous call's `$split` rather than 0, so the recursive call spliced the
manager in one position later each time and handed itself a component list one
element longer. That recursion is unbounded: Cro's `t/http-middleware.rakutest`
aborted the whole file with a stack overflow, with no TAP output at all,
as soon as a second server was built in one process.

With the reset repaired the file runs to completion again — 10 of its 11
subtests pass, leaving only the early-response body blocker already tracked in
`todo/tickets/cro-middleware-await-body-text-dies-coercing-any-into-promise.md`.

The regression came in with the 2026-08-04 `state` work (`a state is one
container shared by its clone's invocations`): clearing the ambient scope for a
method body gave every invocation of a method one stable state key, which is
correct, and in doing so removed the accidental per-caller key variation that
had been papering over the missing cell drop.

Pinned by four new cases in `t/state-var-per-block-clone.t`.
