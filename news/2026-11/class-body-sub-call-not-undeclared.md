# A class body's statements can call the class's own subs again

The BEGIN prologue moves the statements of a `class` or `package` body into a
separate run-time block. The CHECK-time undeclared-routine scan walked that
block as a scope of its own, with no enclosing scope. So a `sub` declared in
the class body was not visible to it, and
`class C { sub f($y) { 1 }; f(1) }` failed with "Undeclared routine". rakudo
runs it fine.

Until #13441, any `use` inside a `unit class` made the check skip the whole
unit, which hid the bug in most modules. Now that a nested `use` only makes its
own scope opaque, the check ran and Timezones::ZoneInfo stopped loading
("Undeclared routines: isleap, seconds-from-time"). Timezone::Simple and
App::samaki, which depend on it, stopped loading too.

The scan now remembers the frame it opened for each class or package body.
It walks the body's run-time block as a scope nested in that frame. The
`Visit` half of the scan moved to `scan/visit.rs` to keep the file under 500
lines.
