# `my @array = lazy { LIST-EXPR }` stores an unforceable `lazy(...)` thunk element instead of a lazy list

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/statement-prefixes.rakudoc:32`).

## Root cause hypothesis

The `lazy` statement-prefix applied to a block (`lazy { (^3).map( *² ) }`) should
produce a lazy `Seq`/list whose *elements* are individually lazy-realized, so that
assigning it to `my @array` stores an array whose elements get filled in on demand.
`say @array` before forcing should show `[...]` (unrealized), and `@array.eager` should
force and flatten it to `[0 1 4]`.

mutsu instead appears to wrap the *entire block result* as a single `LazyThunk` value
and stores that as one element of `@array` (see `src/value/display.rs` around line
1084-1093, `ValueView::LazyThunk` displaying as the placeholder string `"lazy(...)"`
when unforced). Both plain `say @array` and `@array.eager` show `[lazy(...)]` — the
`.eager` call is not forcing-and-flattening the thunk into the array's actual element
sequence; it just leaves the single opaque thunk element in place (and even the
un-eager print renders `lazy(...)` as if it were a value, not raku's generic `...`
not-yet-computed marker).

## Minimal repro

```raku
my @array = lazy { (^3).map( *² )  };
say @array;
say @array.eager;
```

- `raku`:
  ```
  [...]
  [0 1 4]
  ```
- `mutsu` (`target/debug/mutsu`):
  ```
  [lazy(...)]
  [lazy(...)]
  ```

## Relationship to the existing Lazy-list cluster

This is related to (but not identical to) the already-**Deferred** lazy-list cluster's
"closure_seq / scan_spec arrays stay force-capped on `@`-assign" residue noted in
`docs/doc-diff-backlog.md`'s Deferred section — both are about a lazily-computed value
not surviving `@`-array assignment intact. This finding is specifically about the
`lazy { BLOCK }` statement-prefix form (as opposed to a `...` sequence operator), and
about `.eager` failing to force a `LazyThunk` at all (not just failing to share the
reifier across a `.clone`), so it is filed separately in case the fix site differs.

## Affected files (starting point)

- `src/value/display.rs` (~line 1084) — `ValueView::LazyThunk` display.
- Wherever `lazy { BLOCK }` statement-prefix is compiled/executed, and wherever
  `.eager` is implemented for arrays containing a `LazyThunk` element (grep for
  `LazyThunk` and `is_value_lazy` across `src/runtime/` and `src/vm/`).
