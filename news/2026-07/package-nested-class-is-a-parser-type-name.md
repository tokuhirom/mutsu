# A class declared inside a `package` block is now a type name to the parser

`when X::Foo::Missing { … }` failed to **parse** when `X::Foo::Missing` was
declared as a class nested inside a `package` block rather than with a
fully-qualified `class` declaration. The error was `X::Comp::Group: Missing
block` — the parser did not recognise the name as a type, so the `when` never
found its block.

```raku
package GLOBAL::X::Foo {
    class Missing is Exception { method message { "missing" } }
}
try {
    die X::Foo::Missing.new;
    CATCH { when X::Foo::Missing { say "matched" } default { .rethrow } }
}
```

raku printed `matched`; mutsu died at parse time.

## Root cause

The guard that produced the error, in
`src/parser/stmt/control/given_when.rs`, is correct and deliberate: an
undeclared bareword before a block really does gobble it in raku, and this
reproduces Rakudo's `X::Syntax::BlockGobbled` + `X::Syntax::Missing` pair. It
only fires in the `X::` / `CX::` namespaces, which is why the bug surfaced on
exception types and nowhere else.

The defect was on the declaration side. Every declarator registered the
**literal name as written**, so `package GLOBAL::X::Foo { class Missing { } }`
registered only `Missing`, and `is_user_declared_type("X::Foo::Missing")` was
false. Raku installs a nested declaration under its *composed* name, and the
parser had no notion of an enclosing package path at all — grepping for
`current_package` or `package_stack` in `src/parser/` found nothing.

Referencing the type already worked (`X::Foo::Missing.^name` printed the right
thing): the class *was* registered at runtime under its composed name. Only the
parser's "is this token a type name?" table missed it.

## The fix

Three layers, all in the parser:

1. **A package-path stack.** The `package`/`module`/`class`/`grammar` block
   declarators push their name for the duration of their body (via a `Drop`
   guard, so the error paths out of the body parser pop it too), and
   `register_user_type` now also registers `<path>::<name>`. The leading
   `GLOBAL::` pseudo-package is stripped, since it is not part of the composed
   name. The composed spelling goes into the **outermost** scope so it outlives
   the declaring body's lexical scope, matching where Raku installs it. Role
   bodies deliberately do *not* push a path: the scope inside a role is generic,
   so Raku refuses to install an `our`-scoped declaration there.

2. **Composed names across a `use`.** `collect_module_type_names`, which teaches
   the importer about a used module's types, now threads the same package prefix
   through its recursion and registers both spellings. It also recurses into
   class and role bodies, which it previously skipped.

3. **`need` loads types, and transitively.** `package X::Foo { … }` installs into
   `GLOBAL`, so its types are visible to whoever loads the module — including
   through an intermediate module that merely `use`d or `need`ed it. `need
   Module;` was a parse-time no-op, and `extract_exported_names` discarded
   everything the nested parse had registered. Now `need` scans the module for
   type names (without importing its exports), and `extract_exported_names`
   harvests the qualified names its nested parse registered before dropping
   those scopes.

## Impact

This was the largest single blocker for the `DBIish` battery
(`docs/batteries/database.md`). `DBIish` declares all of its exception types
this way and `DBIish::CommonTesting` dispatches on them in a `CATCH`, so the
module failed to parse and took the three SQLite test files with it. All four
now parse and reach their TAP plan; what stops them next is unrelated, and is
recorded in `todo/tickets/dbiish-blockers.md`.

`package X:: { class … }` is the idiomatic way to declare a family of exception
types, so the blast radius is much wider than that one dist.

Pinned by `t/package-nested-type-name.t`.
