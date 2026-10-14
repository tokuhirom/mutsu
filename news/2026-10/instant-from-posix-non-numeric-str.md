# `Instant.from-posix` dies on a string that is no number

`Instant.from-posix("not-numeric-value")` answered the instant at 0 instead of dying with
`X::Str::Numeric`, as Rakudo does. App::RaCoCo's coverable index reads each timestamp with
`try Instant.from-posix($timestamp)` and drops the line when it fails, so a line with a bad
timestamp stayed in the index and was written back on the next flush
(`t/08-coverable-index.rakutest`). The argument now goes through `check_str_numeric` first.
