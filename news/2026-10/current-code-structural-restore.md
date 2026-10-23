# `current_code` is restored on every frame exit

The JIT entries (`try_enter`, `try_enter_range`), the call-frame body loops and
`run_inner_guarded` left `Interpreter::current_code` pointing at the callee's
chunk. A chunk dropped on return left a dangling address that Rust code after
the call could read through `running_code()` (the suspected cause of the
intermittent release-only SIGSEGV in #14483). Each frame run now saves the
caller's address and puts it back on exit.
