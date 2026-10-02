# Assigning Nil resets a by-name scalar to Any

Assigning `Nil` to an untyped scalar resets it to `Any`, and mutsu already did
that when the store went through a local slot. A store that reaches the
variable by name skipped the reset and left a raw `Nil` behind. That covers a
sub, closure or method writing a captured `$x`, an `our` variable written from
a routine, and the run-time half of a class or module body that the BEGIN
prologue splits whenever the file contains a `BEGIN`. So
`class A { my $x = Nil; say $x.raku }; BEGIN 1` printed `Nil` where rakudo
prints `Any`. The by-name store now applies the same reset as the slot store.
It still skips binds, raw parameter stores and declarations, and `$/` and `$!`
keep their `Nil` default (#10608).

The name-keyed `is default(...)` table can still leak a default into a
same-named split class body; that is tracked as #10796.
