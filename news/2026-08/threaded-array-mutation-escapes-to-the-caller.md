# A routine-local `my @a` is no longer the caller's once a thread has run

After *any* thread had run in the process, a routine's own `my @a` stopped being
private: a mutating array method called on it from inside a nested `sub` wrote
through to the caller's same-named lexical.

```raku
sub inner-push() {
    my @arr;
    sub bump() { push @arr, 'x' }    # nested sub closing over @arr
    bump();
    @arr.join(",")
}

my @arr = <SENTINEL>;
await start { 1 };                   # any thread at all arms the shared lane
say inner-push();                    # x          -- right
say @arr.join(",");                  # x          -- WRONG, raku says SENTINEL
```

The same held in the other direction: a worker thread's own `my @a` was the
parent's `@a`.

## Root cause

The cross-thread shared store is keyed by **bare name**, and
`shared_vars_active` never returns to false once set
(`src/runtime/runtime_thread.rs`). So from the first spawn onward, *every* plain
lexical container in the program was funnelled into a single process-wide lane
per name, which cannot represent two concurrently-live bindings of one name.

Concretely, `my @arr` in a routine:

1. wrote the fresh binding into `shared_vars["@arr"]` via `set_shared_var`
   (`box_decl_local_container_cell` published the per-binding `ContainerRef` cell
   there too), overwriting the caller's entry; and
2. every later `@arr.push` / `push @arr, …` took the shared-array fast path in
   `exec_call_method_mut_op` / `exec_array_push_op`, which resolves its receiver
   *by name* out of `__mutsu_atomic_arr::@arr` and ignores the value it was
   handed.

Scalars had been isolated from this since the `thread_redeclared_vars` mask
landed, but `@`/`%` were deliberately excluded from it because those names back
the name-keyed atomic element stores that concurrent `push`/element-assign need
maintained.

## Fix

Plain lexical `@`/`%` declarations now join the `thread_redeclared_vars` mask,
and every container route that decides "is this name genuinely shared?" by
*presence* in the store asks `container_name_is_redeclared` first:

- `set_shared_var_sym` / `sync_shared_vars_to_env` (already consulted the mask),
- `exec_array_push_op` and the mutating-array fast path in
  `exec_call_method_mut_op`,
- `push_to_shared_var`, `assign_array_elem_to_shared_var`,
  `assign_hash_elem_to_shared_var`, `array_name_is_shared`,
- the read preferences in `exec_get_local_op` and
  `get_env_with_main_alias_inner`.

The mask is not permanent: the next `clone_for_thread` force-`declare`s the
binding's current value into the lineage and drops the mask, so a container that
really is shared across threads keeps working — including one declared *after*
the first spawn.

Denying a container the name lane exposed one latent hole, which is fixed in the
same change: `try_native_array_mut` bailed out unconditionally under
`shared_vars_active`, leaving `append`/`prepend`/`unshift` on a frame-local
container to the interpreter fallback, whose plain
`env.get_mut(name).with_array_mut(…)` does not descend the `ContainerRef` cell
`box_decl_local_container_cell` installs — so it silently rebuilt a detached
array and the mutation vanished. It now handles re-declared names, descending
the cell through `env_root_descended_mut`.

## Effect on the vendored Cro::HTTP suite

`Cro.compose` has a method-local `my @components` that a nested
`sub push-component` pushes to, and `Cro::ConnectionManager.BUILD` takes a
`:@components` named parameter. Once the first request had run, `compose`'s
local *was* the unit's `@components` (same `.WHICH`), so every test that
constructs a second `Cro::HTTP::Server` died with

```
Components controlled by a connection manager must compose to form a transform or a sink
```

That error is gone from the whole suite. `http-middleware`, `http-auth-basic`,
`http-auth-basic-with-session`, `http-session-inmemory`,
`http-session-persistent` and `router-auth` now run to completion and fail on
later, unrelated causes.

Pin: `t/thread-callee-array-does-not-clobber-caller.t` (14 tests, green under
`raku` too), alongside the scalar pin
`t/thread-callee-param-does-not-clobber-caller.t`.

## Still open

The secondary anomaly recorded with the original finding is not addressed here:
a `for 1..3 -> $i { say "round $i"; …Cro request…; say "round $i status" }`
printed `round 2 status` in every iteration. It does not reproduce
synthetically and may be the scalar-side sibling of the same aliasing;
re-check it against the fixed array half before chasing it separately.
