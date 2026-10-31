# `::?CLASS` of a role pun with a named parameter default matches its instances

`role C[::T = Any, :$c = 5] { method m(::?CLASS:D:) {...} }; C.new.m` died with
"Invocant ... must be an object instance of type" (and `self ~~ ::?CLASS` was
False): the pun class is registered under a name that carries the named
parameter (`C\0default[Any,:c(5)]`), but the type match trimmed named arguments
from the constraint (#15504) before comparing. A spelling that is itself a
registered class is now taken as is. Found via Concurrent::PriorityQueue
(`push` of its `::?CLASS:D:` multis); `t/02-concurrent.rakutest` now gives the
right result, but needs #15782-style speed (see the PR body).
