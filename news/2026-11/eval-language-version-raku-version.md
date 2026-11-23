# `EVAL q[use v6.c; $*RAKU.version]` answers `v6.c`

An `EVAL`'d unit's `use v6.x` reached the compiler but not `$*RAKU`, which
kept reporting the calling unit's revision. As in Rakudo, `$*RAKU` (and
`$*PERL`) is one process-wide object whose `version` is the language version
of the unit compiled last: an EVAL with its own pragma sets it, and an EVAL
without one compiles at the caller's revision and sets that (#18867).
