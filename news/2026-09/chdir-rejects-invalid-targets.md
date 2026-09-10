`chdir` and `indir` now report their `X::IO::Chdir` failure when a bare call
ends a program. Missing directory leaves, missing root components, and regular
files are rejected with Raku-compatible messages, while value-position calls
continue to return a `Failure` without changing `$*CWD`.
