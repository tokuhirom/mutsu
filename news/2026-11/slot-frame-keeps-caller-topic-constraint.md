# A slot-frame method call no longer strips the caller's typed `$_`

A leaf method that the planner admits to the slot method frame (ADR-18393)
runs on its caller's env handle, with no env of its own. Every method entry
drops the typed-`$_` constraint the callee inherited from its caller. On the
slot frame that drop removed the constraint from the caller itself.

After `sub f(Int $_ is copy) { $obj.leaf(1); $_ = 'str' }`, a release build
therefore accepted the `Str` assignment, where rakudo throws
`X::TypeCheck::Assignment`. A debug build panicked on the frame's
env-unchanged assertion instead. PDF's `t/write-ast.t` hit that panic through
`multi method write(Any:U $_) { $.write-null($_) }` (#19146).

The slot frame now pushes its routine frame without the drop (an `on_caller_env` flag on the method-frame push). Its body never
names `$_`, and it has no env of its own to drop the constraint from. The
method-frame push moved out of `accessors_stack.rs` into
`method_routine_frame.rs`.
