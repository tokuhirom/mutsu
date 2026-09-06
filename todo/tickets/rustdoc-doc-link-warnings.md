# `cargo doc` emits 210 warnings, and no CI job looks at them

## What

```
cargo doc --no-deps --document-private-items
...
warning: `mutsu` (lib doc) generated 210 warnings
```

Nothing in `.github/workflows/` runs rustdoc, so this surface has never been
gated and has drifted for as long as the crate has had doc comments. The
compile-warning sweep of 2026-09-06 (`news/2026-09/compile-warnings-in-unlinted-configurations.md`)
closed the rustc/clippy blind spots and left this one deliberately, because it
is a different, much larger, docs-only diff.

## The breakdown

| Count | Warning | Cause |
| --- | --- | --- |
| 154 | `unresolved link to ...` | two distinct causes, below |
| 37 | `unclosed HTML tag ...` | Raku angle-bracket syntax in prose: `%h<key>`, `Array[Type]`, `sub foo(--> Int)` |
| 15 | `public documentation for X links to private item Y` | a `pub` item's doc links to a `pub(crate)`/private one (`ValueRepr`, `NanBox`, `AttrMap`, `crate::repl_core`, ...) |
| 4 | `this URL is not a hyperlink`, `redundant explicit link target` | bare URLs and `[`x`](x)` duplication |

The 154 unresolved links split into:

- **Raku code read as a link.** `[NaN]`, `[&func]`, `[0;0]`, `[index]`,
  `[2;1]`, `[Z*]`, `[min]` — prose that needs backticks, not brackets.
- **A real intra-doc link written without a path.** `[`alloc_local`]`,
  `[`hoist_sub_decls`]`, `[`set_env_with_main_alias`]` and ~120 more: rustdoc
  does not resolve a bare *associated item* name, so these need `Self::`
  (or `Type::` / a module path when the target lives elsewhere). Every one of
  them is a link the author meant to work and that silently does not.

## Why it is a ticket and not a one-liner

The mechanical part is large (~100 files) and the judgment part is per-site:
which `Self::`, which module path, and which brackets were never meant to be a
link at all. A blind regex pass would produce links that resolve to the wrong
item, which is worse than a warning. A couple of the targets are `#[cfg(test)]`
items (`NativeMethodRow`) or genuinely private modules another module cannot
name (`runtime::receiver_class`), where the right answer is to stop linking.

## Suggested shape

1. Fix the four small categories first (`unclosed HTML tag`, `redundant
   explicit link target`, `this URL is not a hyperlink`, the 15 private-item
   links) — those are unambiguous.
2. Then the 154 links, in passes by directory, checking each target resolves to
   the item the sentence means.
3. Then gate it, so it cannot come back:
   `RUSTDOCFLAGS="-D warnings" cargo doc --no-deps --document-private-items`
   as a step in the `lint-configs` job (it has the headroom), and a `make lint`
   line beside the three clippy configurations.

Do (3) in the same PR as the last of (2) — a gate added before the backlog is
drained just turns every PR red.

## Repro

```
cargo doc --no-deps --document-private-items 2>&1 | grep -c '^warning'
```
