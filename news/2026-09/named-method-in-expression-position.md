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

Parsing the named form correctly exposed a masked bug.
`t/vm/destroy-latch-late-registration.t` wrote
`my $late = method DESTROY { @events.push("early") }`, which used to misparse
as the bareword `method` followed by an immediately-run bare block -- the block
pushed `"early"` itself, so the test passed without any DESTROY firing. With the
method really built and added through `^add_method`, it did not fire: instance
death ran only DESTROYs flagged as submethods, so a plain `method DESTROY`
(declared in a class body or added through the metamodel) never ran at all,
where rakudo runs each MRO class's own DESTROY in either spelling. The
submethod-only filter in `run_pending_instance_destroys_inner` is gone; the
latch test now uses `anon method` plus `^compose` and passes for the right
reason, and `t/oo/method/destroy-plain-method.t` pins the plain-method spelling.
