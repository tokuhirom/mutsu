# `X::Comp` positions, `CompUnit.add-sorry`, and `static`'s fail-on-* modes

`X::Comp` is now a real role where a unit names it: `filename`, `line`,
`column`, `pre`, `post`, `modules`, `highexpect`, `is-compile-time`,
`SET_FILE_LINE`, `SET_PRE_POST`, `at-line` and `sorry_heading`, with the
`===SORRY!=== Error while compiling FILE` rendering of `.gist` (maintainer
decision on #15666). `RakuAST::CompUnit.add-sorry` / `add-worry` queue an
exception on the unit, stamped with the first token's position as rakudo does
(the caller may overwrite it afterwards); when the compile phase ends a lone
sorry fails the compilation as itself, several are bundled under one
`===SORRY!===` (with "Other potential difficulties" for worries), and worries
alone print "Potential difficulties" and let the program run. A unit whose only
compile-time code is a `CHECK` phaser now ends its compile phase too.

Along the way, what `static` 0.0.1 needs of the rest of the toolchain:
`Origin::Source.prepost-of-pos`, `Method.compile-time-value` (and a package
declaration answering `is-resolved`), the `Infix.operator` accessor in `.^can`,
`-c` keeping the unit's class and role declarations (CHECK code looks their
methods up), a native method object being its own sole candidate with `Mu` as
the package of the methods a user class inherits, and `run :err($handle)` /
`:in($handle)` wiring the child's stderr and stdin to a file handle.

`static`'s `t/01-basic.rakutest` (28/28) and `t/14-cli.rakutest` (33/33) now
match rakudo.
