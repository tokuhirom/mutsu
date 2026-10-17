# `Mu.gist` and `Mu.raku` are rows

What a value renders as when its own type declares no `gist` or `raku` is now
`Mu`'s two rows of the built-in method table, reached from every call path
the way `Mu.Str` is. The zero-argument rendering cascade that answered these
names by matching them is gone. `IO::Handle`'s rendering, which was a row
registered under `Mu` and so shadowed `Mu.raku` for other receivers, is a
row of `IO::Handle` now.
