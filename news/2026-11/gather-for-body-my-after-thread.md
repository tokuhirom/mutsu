# A lazy gather's `for` body keeps its per-iteration `my` after a thread has started

Once any thread had been started, `(lazy M())[^4]` over `gather for 1..* -> $j { my $u = $j * 10;
take $b; $b = $u }` read the FIRST iteration's `$u` for ever after (Math::Sequences' Pi digits).
Suspending the gather inside a `for` popped the loop's declaration scope like a finished loop
and restored the body-local `my` to the value it had shadowed, which in a resumed run is the
previous iteration's own binding (the env mirror of `$u` exists only off the env-free declaration
fast path, which a started thread turns off). The `for` suspend sites now use
`suspend_loop_local_scope` (as `while`/`loop` do since #17019). (#17033)
