# `say gather {...}` no longer swallows an exception thrown by the body

An exception thrown inside a `gather { ... }` block propagated correctly
when the resulting `Seq` was reified eagerly (`my @r = gather {...}; say
@r`), but was silently discarded — printing an empty line and exiting 0 —
when the `Seq` was consumed lazily and directly (`say gather {...}`).

`say`'s `.gist` dispatch on a `LazyList` forces the gather body and
re-dispatches onto the resulting `Seq`. `render_gist_value` treats a
`X::Method::NotFound`/`X::Multi::NoMatch` error from that dispatch as "no
`.gist` candidate exists" and falls back to the native placeholder gist —
correct for a type that genuinely has no `.gist` method, but wrong here: a
`LazyList` always has a `.gist` route (either the lazy placeholder or
force-and-redispatch onto a `Seq`, which always has one), so an error
surfacing at that point can only have come from running the gather body
itself, e.g. `say gather { "u".nosuchmethod }` throwing because `Str` has
no `nosuchmethod`. The fallback now excludes `LazyList` targets, so the
underlying exception propagates instead of being mistaken for "no
`.gist`".

`put`/`print`/string interpolation on a lazily-consumed gather have the
same underlying gap (tracked by a pre-existing `TODO` on
`render_str_value`, which returns a plain `String` rather than a
`Result` and cannot propagate at all) and are not fixed by this change.

See [#8159](https://github.com/tokuhirom/mutsu/issues/8159) and the
regression test `t/exceptions/gather-lazy-consume-propagates-exception.t`.
