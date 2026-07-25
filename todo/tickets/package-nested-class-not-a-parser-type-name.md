# A class declared inside a `package` block is not a type name to the parser

`when <Qualified::Name>` fails to **parse** when `Qualified::Name` was declared as
a class nested inside a `package` block rather than with a fully-qualified `class`
declaration. The error is `X::Comp::Group: Missing block`, i.e. the parser did not
recognise the name as a type and so never found the `when` block.

## Repro

```raku
package GLOBAL::X::Foo {
    class Missing is Exception { method message { "missing" } }
}
try {
    die X::Foo::Missing.new;
    CATCH {
        when X::Foo::Missing { say "matched" }
        default { .rethrow }
    }
}
```

```
raku:  matched
mutsu: Runtime error: X::Comp::Group: Missing block
```

## What it is NOT

Each of these was checked in isolation against raku, and none is the trigger:

- **The `use` boundary.** It fails identically when the `package` block and the
  `CATCH` are in the same file, and when the classes come from a `use`d module.
- **A union matcher.** `when A | B { }` is not needed — a single `when` fails.
- **Two `when` clauses.** One is enough.
- **`CATCH` specifically.** A plain `given`/`when` on the same name fails the
  same way.
- **The qualified name itself.** Declaring the very same name directly —
  `class X::Foo::Missing is Exception { }` — parses and matches fine. So does
  `package X::Bar { … }` without the `GLOBAL::` prefix; both spellings of the
  nesting fail.

And note that **referencing the type works**: `say X::Foo::Missing.^name` prints
`X::Foo::Missing`. The class *is* registered at runtime under its composed name —
it is only the parser's notion of "is this token a type name?" that misses it.

## Where to look

Whatever table the parser consults to decide that a `::`-qualified identifier in
term position is a type (rather than, say, a function call, which is what would
make it swallow the following block and then report a missing one). A direct
`class A::B { }` declaration populates it; composing the name from an enclosing
`package`/`module` block declaration does not. The fix is to register the
composed name at declaration time, from the package-scope stack, rather than only
from the literal text of the `class` declarator.

## Impact

This is the single biggest blocker for the `DBIish` battery
(`docs/batteries/database.md`): `DBIish` declares all of its exception types this
way —

```raku
package GLOBAL::X::DBIish {
    class LibraryMissing is Exception { … }
}
```

— and `DBIish::CommonTesting` dispatches on them in a `CATCH`, so the module
fails to parse and takes **three** SQLite test files with it. Replacing the two
`when` matchers with built-in types makes the module load, which confirms the
diagnosis.

The construct is ordinary Raku and `package X:: { class … }` is the idiomatic way
to declare a family of exception types, so the blast radius is much wider than
that one dist.
