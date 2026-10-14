# A closure sees the outer variable after a nested block's same-named `my`

`my $b = 99; my $f = { { my $b = 3 }; $b }; say $f()` printed `(Any)`: the
nested block's `my $b` hid the outer `$b` from the closure's capture set. The
routine-body half of this was fixed by forcing the capture (#13391), but a
closure could not use it, because the closure's exit writeback copied every
local slot whose name the env holds back into the env, and here that slot is
the nested block's own `my`. Calling the closure would have clobbered the
outer variable.

The writeback now skips a forced outer capture's slot: such a name never owns
a slot of its own (the slot is a sigilless parameter or a nested block's
local), and the captured binding is written by name. With that in place the
capture is forced in closure bodies too, and the example prints `99` (#13396).
