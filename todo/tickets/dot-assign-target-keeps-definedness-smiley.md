# `my Type:D $var .= new: ...` fails — dot-assign target keeps the `:D`/`:U` smiley in the type name

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`,
`Type/Metamodel/Mixins.rakudoc:18,63`).

## Minimal repro

```raku
class Billboard {
    has Str $.advertisement;
}
my Billboard:D $billboard .= new: :advertisement("hi");
say $billboard.advertisement;
```

- `raku`: prints `hi`.
- `mutsu` (`target/debug/mutsu`): dies with
  `X::Method::NotFound: Unknown method value dispatch (fallback disabled): new on Billboard:D`.

Dropping the `:D` (`my Billboard $billboard .= new: ...;`) works fine, isolating the bug to
the definedness smiley specifically combined with `.=`.

## Root cause

`--dump-ast` shows the declaration's `type_constraint` is stored as the whole string
`"Billboard:D"` (smiley baked in), and `parse_my_decl_assign`'s `.=`-desugaring builds the
implicit invocant straight from that string:

```rust
// src/parser/stmt/decl/my_decl_assign.rs, around line 654
Some(c) => Expr::BareWord(c.clone()),
```

So `.= new: ...` desugars to `Billboard:D.new(...)` — a bareword lookup for a package
literally named `"Billboard:D"`, which does not exist, instead of `Billboard.new(...)`.
The array/hash-sigil arms just above it (lines 646-653) already build a *different* bareword
string (`Array[c]`/`Hash[c]`) from the same `c`, so they'd have the identical bug for a typed
`@`/`%` declaration too (e.g. `my Billboard:D @x .= new` would try `Array[Billboard:D]`) —
worth checking/fixing alongside the scalar case.

The type-constraint string needs its trailing `:D`/`:U`/`:_` smiley stripped before being
used as a bareword type-name lookup here (the smiley is a definedness constraint on the
*variable*, not part of the package name `.new` should resolve).

## Affected files

- `src/parser/stmt/decl/my_decl_assign.rs` — the `target_expr` match building the `.=`
  implicit invocant from `s.type_constraint` (lines ~645-665).
