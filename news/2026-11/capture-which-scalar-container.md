# `Capture.WHICH` of a named part held in a scalar is stable

`WHICH` of a scalar container that reached `which_of` (a named argument bound from `my $m = Post`)
fell to the counter fallback, so `|c`'s `.WHICH` differed on every call and the debug build's
method-site lane self-check panicked on Red's `alias` cache key (`c.WHICH`). A container now has
the identity of its content (#16882).
