# A class body's `our &t` can be assigned by a later body statement

`class S { our &t; &t = sub { 3 } }; say &S::t()` died with `Cannot modify an
immutable value`. A class-body statement is compiled in a chunk of its own,
which did not know the body had declared `our &t`. So `&t = ...` was treated
as an assignment to a routine name, which is read-only. The body's `our` code
variables now count as assignable there too. The statement writes the
package variable `&S::t`, so `&S::t()` and `S::t()` call the assigned
routine, as in rakudo (#19152). Calling the bare `t()` from a later body
statement, and assigning `&t` from a method, are still open (#19159).
