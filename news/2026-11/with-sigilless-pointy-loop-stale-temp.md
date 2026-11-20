# `with X -> \d` in a loop sees each iteration again

Since the `with` condition's once-evaluated temp moved into a raw local slot
(#18315), a sigilless pointy parameter on a `with` inside a loop kept the
first iteration's value: `for @s { with $_ -> \decl { ... decl<k> ... } }`
answered the first element's key on every pass. The parameter's declaration
treated the temp as an array-share source and promoted it to a shared
container cell parked in the env, and the next iteration's read of the slot
adopted that stale cell.

The temp is a compiler temporary nothing can reassign through a share, so it
is no longer a share source; the parameter takes the value itself and still
writes through to the hash or array it names. This was the nightly sweep
#111 regression of CSS::Properties (which kept only the first declaration
of every style) and of the CSS::Font::Resources, CSS::Stylesheet,
CSS::TagSet, Dist::META and JSON::Mask suites that depend on it (#18377).
