# `Rakudo::Internals.FILETEST-*`, `Rakudo::Iterator.Empty/.OneValue` and `IO::Path`'s `$!os-path`

Two lizmat modules call rakudo internals verbatim (#16672):

- `Rakudo::Internals.FILETEST-E/D/F/L/R/W/X/Z/S/ES/LE/RW/RWX/MODE/MODIFIED/ACCESSED/CHANGED/CREATED`
  answer as rakudo does (an `Int` 1/0 for a test, a thrown `Failed to stat file` for a missing path
  where rakudo throws), sharing their filesystem queries with the `IO::Path` readers.
- `nqp::getattr($io, IO::Path, '$!os-path')` is the absolute path, so `nqp::stat` on it works.
- `Rakudo::Iterator.Empty` and `.OneValue($v)` exist.

inode-dev-devtype 0.0.1 `t/01-basic.rakutest` (6/6) and roundrobin-slip 0.0.3 `t/01-basic.rakutest`
(8/8) now match rakudo. The rest of `Rakudo::Iterator`'s ~80 helpers stay open on the issue.
