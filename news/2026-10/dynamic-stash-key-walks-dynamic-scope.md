# `DYNAMIC::<key>` walks the dynamic scope

`DYNAMIC::<$x>` now finds a caller's ordinary lexical `$x`, as in rakudo, and
looks every key up exactly as spelled frame by frame from the running one. A
routine's own `my $*d` shadows the caller's `$*d` from the routine's entry on
(it reads `Any` before the declaration runs), and a process-level dynamic such
as `$*OUT`, which lives in no frame, is not found (#13590). Call frames now
record their caller's compiled code, so a lexical held only in a slot is visible
to the walk.
