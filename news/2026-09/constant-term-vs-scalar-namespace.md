# Sigil-less constants get their own namespace, apart from same-named scalars

In Raku `constant b = 256` declares the *term* `b`, and `my $b` (or a parameter `$b`)
declares the variable `$b`. They are different symbols: neither can see nor shadow the
other. mutsu stores a scalar `$b` sigil-stripped, under the key `b` — in a compiled
frame's `local_map` and in the runtime `env` alike — and a sigil-less constant was stored
under that very key, so the two were one symbol
([#9962](https://github.com/tokuhirom/mutsu/issues/9962)):

```raku
constant b = 256;
sub g(Int $b where { say b; True }) { say b }
g(5);          # raku: 256 256   mutsu: 5 5
my $b = 7;
say b;         # raku: 256       mutsu: 7
```

The shape that broke a real distribution is a `where` clause naming both, EC 0.6.6's
ed25519: `multi method new(blob8 $b where $b == b div 8)` read the blob as `b`, failed
the constraint, and dispatch fell through to `Mu.new`.

This is the half #7914 left open: that change moved **enum keys** out of the shared key
space (`news/2026-09/enum-key-vs-scalar-namespace.md`, "Still shared: sigil-less
constants and type names").

## The fix

A namespace, stated once in `src/runtime/term_names.rs`. A sigil-less constant is stored
under its *term key* — the name behind a `\` prefix, Raku's own spelling of a sigil-less
declaration, which no scalar key can start with:

- **The compiler** allocates the constant's local slot under the term key
  (`Stmt::VarDecl`, and the expression-position and block-tail declaration paths that
  read a declaration back by name), and a bareword, an assignment target, a `:=` bind
  source, a method-call invocant or an `OUTER::<b>` key that names an in-scope constant
  resolves to that slot. A same-named `$b` gets a slot of its own, and no longer stops
  the constant from being inlined.
- **The runtime** reaches a constant only through term resolution
  (`Interpreter::term_value` / `term_binding`): `GetBareWord`, `::('b')`, `MY::`/stash
  listings, `EVAL`'s declared-name checks, and every place a bare name written where a
  *type* goes is followed to a constant (`constant HANDLE = uint32` type aliases,
  `multi f(G)` value constraints, `is Alias` container traits, NativeCall field types).
  Those went through one helper, `type_name_binding`, instead of each probing `env`.
- **Exports** are recorded under the term key, which is how the importer tells
  `constant b is export` (a term) from `our $b is export` (recorded as the
  sigil-stripped `b`), and the import installs the term under its key.
- A sigil-less write the compiler cannot resolve at all (`EVAL 'b = 3'`) is emitted
  against the term key and the VM falls back to the plain name when no such term is in
  scope, so the constant's readonly mark refuses it — as before — without the `EVAL`
  compiler having to know the caller's constants.

The storage stays in `env`, for the reason the enum-key change gave: block scopes,
package-block rollback, `our` stores and thread clones all key off it. Unlike the enum
prefix, the term key is deliberately *not* a `__mutsu_` metadata key, because a constant
is a user lexical that closure capture and module-scope snapshots must keep carrying.

## Still shared

- A sigil-less `my \b` binding and a `\b` parameter still share the scalar key space.
- A package-qualified store (`Pkg::b`) is keyed by the spelling, so an `our constant b`
  and an `our $b` of the same package still collide there.
- An exported *type object* (a class, a role) keeps its plain key: that is where the
  short-name type-alias machinery and its transitive-leak scoping look.

Pinned by `t/vm/binding/constant-term-vs-same-named-scalar.t` (with the fixture
`t/lib/ConstantTermExporter.rakumod`).
