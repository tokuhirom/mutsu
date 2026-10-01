# Parser-side analyses walk every position with the typed AST visitor

Sixteen more hand-rolled `Stmt`/`Expr` walkers now implement `crate::ast_visit::Visit`
(ADR-0137, #10468), so each one searches every child instead of the forms its `_ =>` arm happened
to list:

- The scans of a `use`d module that tell the importer's parse what the module declares — its
  exported routines, type and enum-type names, enum values, constants, `EXPORTHOW::DECLARE`
  keywords, a `.define_slang` call and a computed export-stash key — were nine separate walkers
  in `module_exports.rs`, `dynamic_stash.rs` and `enum_values.rs`. They are now one walk,
  `module_exports/decl_scan.rs`, which also takes a share of `module_exports.rs` (1825 → 1148
  lines).
- The inline-package export scans (`import M` after `module M { ... }`) and the export name-clash
  check moved from `package_decl.rs` to `class/export_scan.rs`.
- The `X::Syntax::NoSelf` checks in `class/attr_checks.rs`.
- The `use lib` replay ahead of the BEGIN-time preloads (`compiler/begin_use.rs`).

Every position a port newly reaches was checked against rakudo:

- rakudo's `is export` exports from any depth of a package, so an `is export` routine, token,
  operator alias, constant or enum declared in a routine body, a control-flow block or a nested
  package is now known to the importer's parse (`sub setup { sub infix:<x>(...) is export {} }`
  makes `1 x 2` parse), and two non-`multi` exports of one symbol anywhere in a package raise
  `X::Export::NameClash`. An `our`-scoped type in a routine body is registered under its package
  path. Off the package spine, a lexical type, a routine-local constant and a non-exported enum
  value stay private, so they cannot shadow an importer's routine.
- `$!x`, `$.x`, `@!x` in a `self`-less sub of a class body is rejected wherever rakudo rejects
  it — an array or hash literal, a pair, an `if`/`for` body, a parameter default, a method call
  receiver, an assignment — while a nested `method` declaration or method literal, which brings
  its own `self`, is accepted. The `has $x` alias read at class-body level is rejected in a list,
  an `xx`, a `my $y = $x` initializer; a nested block is still not searched, since it may declare
  a lexical of the same name.
- A literal `use lib` inside a routine body is replayed before a nested `use`'s preload, as
  rakudo puts it into the repository chain at BEGIN time wherever it is written.

The other parser-side rows of `scripts/ast-walkers-baseline.txt` are annotated: they are
lowerings, spines, renderers, recursive-descent parse functions or one-scope declaration scans.
The ratchet fell from 216 to 200 walkers.
