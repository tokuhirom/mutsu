# Multi dispatch: a named-hash slurpy no longer loses to a bare zero-arg candidate

A `multi method` that declares a named parameter plus a named-hash slurpy is
narrower than a bare zero-arg candidate and must win dispatch. In Raku a zero-arg
method is really `(*%_)`, so `(Bool :$b, *%a)` out-narrows it. mutsu ranked the
bare `()` candidate as *more* specific: `method_candidate_type_distance` charged a
flat `+2000` distance penalty to any named-hash slurpy (`*%a`), while the bare
candidate's implicit `*%_` is not in its `param_defs` and paid nothing. So `()`
won on raw distance and the explicit-named tie-break never ran.

The concrete fallout was `HTTP::Request.new(GET => $url)` dispatching to the wrong
`new`: the pair-collecting `multi method new(Bool :$bin, *%args)` lost to a bare
`multi method new()`, so the URL was silently dropped and the client connected to
`localhost`.

The fix stops penalizing a *named-hash* slurpy in the type-distance sum (a
*positional* slurpy `*@x` is still penalized, so `(@x)` keeps beating `(*@x)`). A
named slurpy competes only over named arguments, and the existing narrowness
tie-break already ranks it correctly: a candidate that declares an explicit named
parameter (`(IO::Path :$file!)`) still beats a bare `(*%items)`, and `()` vs
`(*%a)` now correctly *ties* (ambiguous), matching rakudo.

Pinned by `t/multi-named-slurpy-ranking.t`.
