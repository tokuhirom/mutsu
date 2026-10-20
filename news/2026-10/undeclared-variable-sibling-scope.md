# A variable only a sibling scope declares is a compile-time error

`sub f { $x = 1 }; { my $x = 2 }` and `{ my $x }; say $x` are now
`X::Undeclared` at CHECK time, as in rakudo (#12800, slice 3). Before, the
compunit scan left any name some other scope of the unit declared to run
time, where a routine resolved it through its caller's env and could write
the caller's same-named lexical.

The rule stays lenient inside a type or package body and a phaser, where the
AST moves declarations away from their source scope. Getting it right
everywhere else meant teaching the scan the declarations the lowered AST
does not spell as an enclosing `my`:

- a statement prefix over a statement (`try my $v = ...`, `once my $o`)
  declares into the enclosing scope;
- so does the iterable of a `for` and the topic of a `given`
  (`for my @a = 1, 2 { }; @a`);
- a regex's `:my $c` declaration is in scope after the regex;
- a destructuring pointy `given`/`with`/`if` declares its sub-signature's
  parameters, a `|c` capture included;
- a class body's run-time statements (`PackageRuntimeBody`) and a supply
  block's hoisted `CLOSE` registration are judged as the moved code they are.

The scan was run over every `t/` file and the whole roast whitelist. It also
caught three `t/` files that rakudo rejects too, now fixed.
