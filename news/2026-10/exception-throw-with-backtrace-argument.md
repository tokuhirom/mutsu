# `Exception.throw` accepts an optional Backtrace

Rakudo's `Exception.throw` takes an optional Backtrace, and a subclass's
`method throw { nextwith $!orig-backtrace }` is the usual way to rethrow a
mapped exception with the original trace. mutsu's `throw` took no arguments, so
the `nextwith` died with "Too many positionals passed; expected 1 argument but
got 2" and masked the real error: every mapped Red driver exception
(`X::Red::Driver::Mapped`) surfaced that way (#14094). `throw($bt)` now throws
the exception carrying the given backtrace.
