# `Supply.throttle` runs its callbacks concurrently

`Supply.from-list(@items).throttle($n, &code)` ran `&code` for the items one
after the other on the calling thread, so a callback that blocks (`await
$proc.start`, `sleep`) never overlapped with its siblings: Proc::Q's `:3batch`
of three one-second processes took three seconds. Up to `$n` callbacks run at
once now, and their results stay in input order. Proc::Q's five test files
match rakudo (t/04-batch was the failing one).
