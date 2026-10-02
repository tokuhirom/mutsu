# A BEGIN in a class, role or package body, or in one of its methods, runs at BEGIN time

`class A { method m { BEGIN say "in-method" } }; say "m"` printed `m` and never ran the `BEGIN`;
rakudo prints `in-method` first. The `BEGIN` of a role body or method never ran, a `BEGIN` in a
class or package body ran when the declaration ran (so `class C { my $x = 5; BEGIN say $x.defined }`
said `True`, rakudo `False`), and every later nested `BEGIN` of the unit lost its lifting too.

A `BEGIN` in the body of a class or package stays in the body, which runs while the class is being
declared (it may compose a role, `.^add_role`, ahead of the methods that use it); the package is now
declared in the prologue, ahead of the mainline. A `BEGIN` in a method or sub of a class or package
is lifted like an `INIT`: static cells for the method's lexicals, a value slot for the value form,
and the body runs in the class's own context after it is declared (`$?CLASS` is the class). A role,
or a class declared inside a routine, has no package to re-enter, so its `BEGIN`s go straight to
the prologue.

Fixed on the way: the run-time part of a class body declared in the prologue did not swallow the
error of an `EVAL` statement as a class body registered in place does, so
`class C { EVAL q[has $.w] }` threw once any `BEGIN` followed it.

Still open ([#10751](https://github.com/tokuhirom/mutsu/issues/10751)): a class-body statement's
write to an outer `my` that a method of the class reads is lost, with or without a prologue.
