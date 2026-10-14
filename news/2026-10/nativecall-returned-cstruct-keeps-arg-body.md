# NativeCall: a returned CStruct handle keeps its argument's body alive

A native function that returns its first argument (`g_value_init` in Gnome::N)
used to hand back a handle into a body owned by a temporary, as in
`g_value_init(N-GValue.new, ...)`. The temporary was dropped, the handle dangled
and the next native call segfaulted. The returned handle now shares the
argument's body block, so Gnome::N's `t/NativeLib.rakutest` runs to its plan.
