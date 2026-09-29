# An exception in a BEGIN block is reported as a BEGIN, not a CHECK

`BEGIN { die "x" }` used to surface as "An exception occurred while evaluating a CHECK"
because the X::Comp::BeginTime wrapper hardcoded the word. `CheckPhaserStart` now carries
whether it opens a `BEGIN` or a `CHECK` body, the VM keeps a stack of those kinds aligned
with `check_phaser_depth`, and the wrapper names the innermost one (#9917).
