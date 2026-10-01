# A package's `our sub` is installed at compile time wherever it is declared

The CHECK-time prepass that installs a package's routines before the package
body runs used to look only at top-level `package`/`module` bodies. An `our sub`
in a package declared inside a never-run branch or an uncalled routine, or in a
nested block of the package body, stayed unreachable until execution got there:

```raku
say P::g();                           # Could not find symbol '&g' -> 42
package P { if False { our sub g { 42 } } }
```

The prepass is now a typed AST visitor (`src/runtime/inline_package_subs.rs`)
that finds packages anywhere in the unit and, below a package body's own
statement list, collects the `our` routines. Those nested routines are not
marked "already installed", so the in-sequence registration still runs when
their block does and binds the declaring activation's lexicals: before the
block runs the sub sees the lexical as undefined, afterwards the bound value,
as in rakudo. (mutsu#10504)
