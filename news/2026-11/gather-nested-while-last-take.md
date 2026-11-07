# A lazy gather with a `while` nested in a `while` no longer loses its last take

Suspending a gather inside a condition-driven loop popped the loop's declaration
scope the same way a finished loop does, restoring the "shadowed" values of the
body-local `my` names. For a resumed outer loop those saved values were the previous
iteration's own bindings, so the inner loop's `my $k = 0; ... $k++` was rolled back
to a stale value on the next pull and the last `take` vanished. A suspension now
drops the scope without restoring (`suspend_loop_local_scope`); `while`, `until` and
C-style `loop` all use it. (#17019)
