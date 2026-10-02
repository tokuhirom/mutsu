# `use Foo:if(False)` no longer declares Foo's exports to the undeclared-routine check

`use if; use Foo:if(False); foo()` now fails at compile time with rakudo's
`Undeclared routine: foo`, instead of running the mainline and dying with
`Unknown function: foo` (#10331, ADR-0134 §2.1.6).

The parser still registers a conditional `use`'s exports for the rest of the
parse, since it cannot know a BEGIN-time condition. But the statement now
records those names (`Stmt::Use::if_imports`), and the undeclared-routine check
does not count them as declared. The check runs before the BEGIN prologue, so
a call it cannot explain becomes a guard placed right after the prologue. The
guard raises the compile-time error unless one of the unit's conditional
`use`s was loaded. The same applies to a loaded module's own top level.

As part of this, a unit's import-free pragmas (`use if`, `use lib`,
`use strict`, `use MONKEY-*`, ...) no longer switch the check off, so
`use strict; nosuch()` is now a compile-time error too.
