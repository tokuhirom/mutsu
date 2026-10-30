# `Distribution::Path.content` returns an unopened `IO::Handle`

`$dist.content($name-path)` answered an `IO::Path`; zef's `make-install` does
`$handle.close; $*REPO.load($handle.path)` on it and died with "No such method
'close' for invocant of type 'IO::Path'". It is now an unopened `IO::Handle` on the
file (as rakudo's), closing a never-opened handle answers True, and a relative
prefix is no longer joined twice onto the files map's already-prefixed paths (#15180).
