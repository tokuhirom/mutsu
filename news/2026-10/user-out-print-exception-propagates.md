# An exception thrown by a user `$*OUT.print` propagates

`print`, `say` and `put` are `$*OUT.print(...)`. When `$*OUT` was a user object whose `print`
threw, mutsu swallowed the exception and wrote the text to the real stdout anyway, so
`my $*OUT = class { method print(|) { die "no output" } }; print "x"` printed `x` and went on.
Red installs exactly such a handle to forbid output inside a `map` block (and relies on the
resulting `die` to fall back to the native `map`). Only a dispatch failure (the handle has no
usable `print`) now falls back to the real stream; any other exception propagates. Red
`t/41-fallback` subtest 2 now sees "Trying to print inside the map's block".
