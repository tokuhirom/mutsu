# A method call on a parameter no longer keeps a method out of the slot frame

A planned method whose body calls a method on one of its own parameters, such
as `multi method scale(Str $s) { $s.chars }` in `bench-multi-dispatch`, ran in
the lean frame. The call compiles to `CallMethodMut`, which treats its
receiver as a variable it may rebind. It does that by name: a rebind check
before and after the call, writebacks, a raw-invocant cell. The slot frame
(ADR-18393) runs on its caller's env, so those by-name steps would reach the
caller's same-named variable.

A lean parameter is a readonly binding, and nothing may rebind it. So
`slot_frame_body_ok` now admits that call when it has no arguments, no
modifier and an unquoted name other than `VAR`, and its receiver slot is a
parameter. `exec_call_method_mut_site` then runs it as the call on the
parameter's value it is, as long as the op belongs to the innermost slot
frame's own body (`VmCallFrame.slot_body`). A built-in row answers it through
the `CallMethod` site lane, so `$s.chars` costs what it does on a value.

The work found two problems that had been there since the slot frame first
shipped:

- `local_cell_adoption_target` looks in the top env overlay for a container
  of the slot's name. A slot frame has no overlay of its own, so it was offered
  the caller's same-named variable when a closure had captured it. Nothing is
  adopted in a slot frame now.
- The debug-build check that the env handle is unchanged on return no longer
  holds once the body makes calls: a callee's writes, and those of a caller's
  `CATCH` run inline at a throw (ADR-0072), legitimately land in the shared env.
  It now applies to call-free bodies only. This also clears the debug panic a
  method-not-found inside such a body hit after #19267.

The `Str` candidate's call drops from ~14.3k to ~12.3k instructions. A test
pins the calls on parameters, the caller's `CATCH`, `CONTROL` and dynamics,
and the caller variables a closure captured. #19266.
