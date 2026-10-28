# A spawning callee's `my %h` / `my @a` no longer replaces the caller's same-named variable

Once a thread had spawned, a routine whose body declared `my %pars` and then (directly or through a callee) started a thread published its binding under the bare name `%pars` in the cross-thread lane. After the routine returned, its caller's own `%pars` was answered with that entry on the next write or read, so a recursive Red `^create` (whose `save` emits through a `start`) lost the outer `:a(5)` to the nested call's `%pars` (`t/30-update`, `t/16-create-related-pars`, the `t/61-model-as-type` flake).

A returning routine frame now re-masks its own plain `@`/`%` lexicals (`ThreadParamMaskGuard`, which already unmasks the parameters), so the name goes back to the caller's binding. The mask is lifted again at the caller's next spawn, which publishes the caller's current binding as before.

Red 0.2.7 `t/30`, `t/16` and `t/61` (3 of 3 runs) pass. Refs #15020.
