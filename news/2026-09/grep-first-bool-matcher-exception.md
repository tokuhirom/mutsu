# `grep`/`first` on a `Bool` matcher throws a proper `X::Match::Bool`

`(1,2).grep(True)` (a common typo for `(1,2).grep(*.Int ~~ True)` or similar)
correctly raised `X::Match::Bool`, but the exception was built with no
attributes at all. `.type` failed with "No such method 'type' for invocant
of type 'X::Match::Bool'", and the uncaught top-level print showed only the
bare class name `X::Match::Bool` instead of a real message.

All four throw sites (`grep`'s read-only and rw-mutating paths, `first`'s
free-function and method forms) now go through a shared
`RuntimeError::match_bool(routine)` constructor that stamps the `type`
attribute rakudo reports (`.grep` / `.first`) and derives the message text
from the same `format_exception_message()` table entry a hand-built
`X::Match::Bool.new(:type<.grep>)` already used — so a thrown exception and
a hand-built one render identically, and the message now matches rakudo's
wording exactly (`"Cannot use Bool as Matcher with '.grep'.  Did you mean to
use $_ inside a block?"`, including the double space Rakudo's own text uses).

Pinned by `t/exceptions/grep-first-bool-exception.t`.
