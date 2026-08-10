# A slurpy `@`/`%` parameter's thread-mask now survives into a spawned thread's own body

A `*%options` (or `*@items`) slurpy parameter read inside a nested `start {}` block used to
return the FIRST call's value on every later, purely sequential call to the same routine — not a
concurrency bug: the two calls were fully `await`ed one after the other on the same thread.

```raku
sub connect(*%options) {
    start %options<prepend>
}

my $h1 = await connect(prepend => 'un');
my $h2 = await connect(prepend => 'in');
# mutsu printed 'un' / 'un'; raku prints 'un' / 'in'
```

This was the root cause of both `t/composer.rakutest` test 134 and
`t/connection-conditional.rakutest` test 23 in Cro::Core — both call a `Cro::Connector`'s
`method connect(*%options) { start Transform.new(prepend => %options<prepend>) }` (or
`:cond(%options<cond>)`) twice in sequence with different option values, and the second call
read back the first call's option.

## Root cause

`mask_thread_redeclared_params` marks a slurpy `@`/`%` parameter's bare name as call-local
(`thread_redeclared_vars` + `thread_param_shadow_vars`) for the duration of the call, so the
read gate `container_name_is_redeclared` keeps reads of that name local to the call's own
env/locals instead of falling through to the cross-thread shared-variable store. This mask is
lifted (`unmask_thread_redeclared_params`) as soon as the call's *synchronous* body returns.

But a nested `start {}` spawned from inside that body runs its own body on a **cloned**
Interpreter, asynchronously, on another thread — potentially well after the synchronous call
has already returned and unmasked. `clone_for_thread_excluding` (`src/runtime/runtime_thread.rs`)
built the child's `thread_redeclared_vars`/`thread_param_shadow_vars` from scratch (only the
block's own closure-captured scalars, which never include an aggregate), discarding the parent's
active parameter mask entirely. So by the time the spawned block's body actually read
`%options`, the read gate saw an *unmasked* name and fell through to the shared store — which
held whatever the very first call to `connect` had `seed_if_absent`-published under the bare
name `%options` (a no-op on every later call, since the entry was never cleared).

## Fix

`clone_for_thread_excluding` now unions the parent's *currently active* `thread_param_shadow_vars`
into the child's own `thread_redeclared_vars` and `thread_param_shadow_vars`, so the mask
established for a slurpy parameter survives for as long as the CHILD's own body runs — the
child's env was cloned from the parent's env at spawn time, so it already holds this call's own
correct value under that name; the fix simply keeps the read gate from bypassing it.

Fixes `t/composer.rakutest` (134/134) and `t/connection-conditional.rakutest` (23/23) in
Cro::Core. Pinned by `t/slurpy-param-nested-start-sequential-calls.t` and the pre-existing
`t/hash-slurpy-param-thread-mask.t` (unaffected).
