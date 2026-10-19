# A variable the program declares nowhere is a compile-time error

`$xx = 2` in a program that declares no `$xx` anywhere is `X::Undeclared` in
rakudo, raised at compile time before anything runs. mutsu ran the program
and either failed when the statement executed or created the variable (#12800).
The CHECK-time scan added for variables declared only later (#13982) now
reports these as well, with rakudo's "Did you mean" suggestion.

Turning the check on over every `t/` file and the roast whitelist showed
the declaration forms it still had to learn:

- **Order.** The scan runs on the unit in source order, before the BEGIN
  prologue moves `sub`s, classes and `our` declarations to the head.
- **`use Test`.** `Test` exports no variables. The scan gave up after every
  `use Test` before, so the check never applied to a test file.
- **Modules with unlisted exports.** A module whose exported variables the
  parse-time scan does not list makes the check give up: an `our` in an
  `EXPORT::<tag>` stash, or a constant outside every package (a bare-file
  module's, or one before `unit class`).
- **Declarations the AST moves.** A name some inner scope declares is left
  to run time: a method declared in a nested block of a class body is moved
  out of that block. Phaser bodies are not judged against later top-level
  declarations.
- **Smaller forms.** `$self` parameters and lexicals (ADR-0061), sigiled
  terms (`constant term:<$box>`), a pointy `given`'s destructured
  parameters, `has ($a, $b)` aliases and `require M <$x>` all declare.
  Code variables and attributes (`&!cb = ...`), type-like sigil-less
  targets (`Nil = 5`, `C .= new`), Unicode-digit match variables (`$١`)
  and interpolated extended identifiers are not judged.

Two `t/` files used an undeclared variable to test that an `EVAL` or an
import does not leak. Rakudo rejects those at compile time too, so they now
look the name up with `EVAL`.
