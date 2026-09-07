# An `our` variable declared in a block that never runs is installed anyway

```raku
if False { our $o = 4; END { say $o.^name } }
say "mainline";
# raku : mainline / Any     mutsu: mainline / Nil

if False { our $o = 4 }
say OUR::<$o>.^name;       # raku: Any    mutsu: (no such symbol)
```

rakudo installs a package symbol when the compunit is **compiled**, so the `$o`
slot exists (undefined) even though the assignment never ran. mutsu installed
the symbol when execution reached the declaration, so a declaration in a dead
branch installed nothing and a later read found no binding — which reads as
`Nil`.

This is the `our` half of `news/2026-09/unreached-end-phasers-see-unassigned-lexicals.md`,
which covered `my`/`state`; an `our` is a package symbol and took a different
path.

## The fix

The `EndWalker` pre-pass (`src/runtime/end_phasers.rs`) already walks the whole
main compunit — dead branches included — before it runs, precisely so a
never-reached `END` still gets installed, and it already tracks the package the
statements it is walking belong to. So the `our` installation is the same walk:
`EndWalker::declare`, which until now only recorded `my`/`state` lexicals, now
also installs an `our` declaration's package symbol.

The symbol goes into `our_vars`, the flat package-symbol store the stashes
read — **never** into the env. The env is also the *lexical* store, and an
`our`'s lexical alias belongs to its declaring block: putting a bare name there
leaks it out (`{ our $sa2 = 42 }; $sa2` must stay `X::Undeclared`), and putting
a qualified one there perturbs the declaration path that later runs for a
reached declaration (`my $b = 'bt'; module M { our $b = 'bo' }` started
answering `bo` for the mainline `$b`). Both were caught by the existing suite.
`package_stash_value` now also scans `our_vars` for a named package's members,
so a dead-branch `our` inside a `class`/`module` shows up in that package's
stash; a live env value still wins.

The block-scoped half is separate: an `our` in a **nested** block also creates a
lexical alias there, so its name joins the walker's scoped `lexicals` stack and
a never-reached `END` in that block reads it as unassigned, exactly like a `my`.
A unit-top-level `our` is skipped there — it always runs, so a never-reached
`END` must read the real value its package symbol holds
(`our $pkg = 7; if False { END { say $pkg } }` is `7`, pinned by an existing
row).

The two details the ticket flagged both fall out of that:

- **The package qualification.** The walker tracks the NESTED package path
  separately from the `package` an `END` runs under, because those are two
  different things: `package D1 { package D2 { our $d3 } }` stores `D1::D2::d3`,
  and keying the pre-install on the innermost `D2` alone shadowed the real
  symbol for a later `$D2::d3` read (`roast/S04-declarations/our.t` 12/14/16).
  Otherwise the walker's package is the *enclosing*
  package — it descends into `class`/`role`/`module` bodies but not into sub
  bodies — which is exactly what `Compiler::qualify_our_variable_name`
  qualifies an `our` declaration against (it deliberately uses
  `enclosing_package` rather than the `Pkg::&sub/1` state-scope pseudo-package).
  `our_symbol_key` mirrors that one rule, so a dead-branch `our` inside a class
  lands in that class and one inside an uncalled sub lands in its enclosing
  package.
- **Not disturbing a reached declaration.** Only the *symbol* is installed,
  never a value, and only when the name has no binding yet. An assignment that
  does run overwrites the type object through the ordinary declaration path, so
  `if True { our $run = 9 }` still reads `9`.

An `@`/`%` `our` gets an empty container rather than a type object, which is
what rakudo shows (`if False { our @a }; OUR::<@a>` is `[]`), and a `&` name is
left to the routine-declaration walk.

Pinned by `t/our-in-dead-branch-is-installed.t` (9 assertions, all passing under
rakudo) and two new rows in `t/end-phaser-compile-time-install.t`.
