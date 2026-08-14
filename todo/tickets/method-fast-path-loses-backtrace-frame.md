# The method-dispatch fast path never pushes a backtrace `RoutineFrame`

Found while fixing `todo/tickets/repeat-call-loses-backtrace-frame.md` (now
`news/2026-08/repeat-call-loses-backtrace-frame.md`), which was about
`call_compiled_function_fast` (sub calls) losing the `in sub` backtrace frame
on the second and later call to the same routine.

The same class of bug exists for **methods**: `call_compiled_method_fast`
(`src/vm/vm_method_dispatch.rs`) is the specialized fast dispatch path for
compiled methods, analogous to `call_compiled_function_fast` for subs — it
skips `push_call_frame` in favor of `push_light_call_frame`, and, checked
directly, never calls `push_method_routine_with_location` /
`self.routine_stack.push(...)` anywhere in its body. The two OTHER method
dispatch paths (`call_compiled_method` and a second unnamed one, both also in
`vm_method_dispatch.rs`) DO push a `RoutineFrame` via
`push_method_routine_with_location`.

This means a method call that reaches the fast path likely loses its `in
<Class>::<method>` backtrace frame (and `&?ROUTINE`/`CALLER::`/`callframe`
visibility) the same way sub calls did before that fix — needs verification
with a repro that forces a method call through
`call_compiled_method_fast` specifically (check its eligibility gate near its
definition) and then triggers an error inside the method body on a REPEAT
call.

## Why this is a separate, deferred ticket

The RoutineFrame struct was just changed (in the sub-fast-path fix) to use
interned `Symbol` fields instead of `String`, specifically so a frame push is
cheap enough to do unconditionally on a hot path. That work already updated
`push_method_routine_with_location`'s signature to take `Symbol`s. Wiring an
unconditional push into `call_compiled_method_fast` should now be a
comparatively small, well-scoped change — but it needs:

1. A repro that reliably routes through `call_compiled_method_fast` (its
   eligibility conditions need to be read carefully — see the guards near its
   definition in `vm_method_dispatch.rs`).
2. Verification that the fix doesn't regress the method fast path's
   performance characteristics the way `call_compiled_function_fast`'s did
   (the earlier fix's whole approach was designed around avoiding a
   regression there; `Symbol` interning should make it equally cheap for
   methods, but this needs its own perf sanity check).
3. A pinned `t/*.t` test analogous to `t/repeat-call-backtrace-frame.t`, but
   for a method called via the fast path.

Keeping this as its own PR/ticket avoids further scope creep in the sub-path
fix and lets each change get its own focused verification.
