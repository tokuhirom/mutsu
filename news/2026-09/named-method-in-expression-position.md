# A named `method NAME (...) { }` parses in expression position

`has @.x = method TOP ($/) { ... };` failed with `Confused. Two terms in a row`
at `TOP`: the primary-term parser accepted only the anonymous method literal
(`method (...) { }` / `method { }`), never a named one. In the mainline,
`my $m = method foo ($x) { ... }` misparsed as a call of `method` and printed a
spurious `Useless use of "*"` warning.

In rakudo a routine declarator in expression position is still a declaration.
Inside a package body `method TOP` installs `TOP` as a method of the enclosing
class (or role), *and* the expression evaluates to that method object; in the
mainline it only yields the object.

Both halves are now implemented. The expression parses as a lexical
`my method`, which already evaluated to the named `Method`. A copy of the
declaration is recorded in a new parser frame (`parser/stmt/hoisted_methods.rs`)
that `package_body_block` opens around every class/role/grammar body and
drains into it as an ordinary method statement once the body has parsed. The
parser backtracks, so entries are keyed by the source position they were parsed
from and a repeat is dropped. The `submethod` spelling works the same way.

Found via `Raylib::Bindings` (`lib/Raylib/Actions.rakumod:38`), whose actions
class writes `has @.ignored-functions = <...commented-out list...> method TOP
($/) { ... }` -- with the list items commented out, the method becomes the
initializer, and a grammar using the class as actions has to find `TOP`.
Part of the #7988 parse-gap cluster; pinned by
`t/oo/method/named-method-expression-position.t` (issue #9474).
