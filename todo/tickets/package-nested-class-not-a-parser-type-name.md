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

## Where it is — exact code sites

The guard that produces the error is
[`src/parser/stmt/control/given_when.rs:43`](../../src/parser/stmt/control/given_when.rs):

```rust
if rest.starts_with('{')
    && let Expr::BareWord(name) = &cond
    && (name.starts_with("X::") || name.starts_with("CX::"))
    && !crate::runtime::utils::is_known_type_constraint(name)
    && !crate::runtime::utils::is_known_compound_type(name)
    && !crate::parser::stmt::simple::is_user_declared_type(name)
{
    return Err(gobbled_block_error(name));
}
```

The guard itself is correct and deliberate — an undeclared bareword before a
block really does gobble it in raku, and this reproduces Rakudo's
`X::Syntax::BlockGobbled` + `X::Syntax::Missing` pair. Note it only fires for the
`X::` / `CX::` namespaces, which is why the bug shows up on exception types and
nowhere else.

The defect is in the third check. `is_user_declared_type` /
`register_user_type` live in
[`src/parser/stmt/simple/pragma_preseed.rs:50-70`](../../src/parser/stmt/simple/pragma_preseed.rs)
and are backed by a scope-stack of name sets. Every declarator registers the
**literal name as written**:

- `src/parser/stmt/class/class_decl.rs:466` — `register_user_type(&name)`
- `src/parser/stmt/class/package_decl.rs:387`, `role_decl.rs:451`,
  `grammar_module.rs:177`, `decl/constant_subset.rs:229,333`,
  `decl/my_decl_dispatch.rs:96` — same shape

So `package GLOBAL::X::Foo { class Missing { } }` registers only `Missing`, and
`is_user_declared_type("X::Foo::Missing")` is false.

## Suggested fix

Register the **composed** name in addition to the literal one. The parser has no
package-path stack today (grepping for `current_package` / `package_stack` in
`src/parser/` finds nothing), so the fix needs one: push the declared package
name in `package_decl` (and the `module`/`class`/`role` block declarators) while
their body is parsed, and have `register_user_type` also insert
`<enclosing path>::<name>` — stripping a leading `GLOBAL::`, which is a
pseudo-package and not part of the composed name.

Prefer that over relaxing the guard: the guard is what gives the correct Rakudo
error for genuinely undeclared names, and weakening it would trade a wrong
rejection for a wrong acceptance.

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
