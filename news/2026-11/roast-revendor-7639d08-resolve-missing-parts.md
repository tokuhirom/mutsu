# roast re-vendored to 7639d08; `.resolve` walks past missing path parts

`roast/` now tracks Raku/roast master at 7639d080ee3f (2026-11-11). Upstream
added six `is copy` from read-only source tests to `S06-traits/misc.t`, which
mutsu already passed, and un-todoed "absolute inverts relative with resolve"
in `S32-io/io-path-unix.t` (plus a root-resolves-to-itself test).

That un-todoed test exposed a real `IO::Path.resolve` bug. mutsu stopped
resolving at the first path part that does not exist and appended the rest
verbatim, so `"/zip/loc/../../foo/bar".IO.resolve` stayed as written instead
of becoming `/foo/bar`. The walk now follows rakudo's: a missing part is kept
as written, a later `..` still drops it, and existing parts are canonicalized.
`.resolve(:completely)` failures also carry rakudo's reason
(`Doesn't resolve to an existing path`, `No parent dir to go back to`) in the
message and in `X::IO::Resolve.reason`.

The re-vendor also removes a stray empty `roast/-` file that an earlier PR
committed by accident; upstream has no such file.
