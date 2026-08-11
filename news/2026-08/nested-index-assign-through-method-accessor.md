# `$obj.accessor[i][j] = v` now writes through a plain Array/Hash attribute accessor

A nested subscript assignment through a no-arg method accessor —
`$obj.method[i][j] = v` or `$obj.method<k>[i] = v` — silently dropped the
write whenever the accessor returned a plain Array or Hash (rather than an
`Instance` with its own subscript protocol):

```raku
class Table {
    has @.cell;
}
my $t = Table.new(cell => [[1, 2, 3], [4, 5, 6]]);
$t.cell[0][1] = 48;
say $t.raku;   # was: Table.new(cell => [[1, 2, 3], [4, 5, 6]]) -- now: Table.new(cell => [[1, 48, 3], [4, 5, 6]])
```

`builtin_index_assign_method_lvalue_nested`
(`src/runtime/builtins_multidim_assign.rs`), which backs this call shape,
already handled the `Instance`-returning case (dispatch through
`AT-POS`/`AT-KEY`/`ASSIGN-POS`) and the typed-attribute autovivification
check, but the actual write for a plain container fell through to a `TODO:
implement proper nested assignment for non-Nil elements` that just discarded
the value. mutsu's containers are copy-on-write (no interior mutability for
element cells), so writing through two subscript levels needs the same
"read, rebuild by value, write back through the accessor's setter" shape the
single-level `builtin_index_assign_method_lvalue` already used — applied
twice (rebuild the inner element, then rebuild the outer container around
it), including the existing shared-Arc propagation to other aliases of the
same container.

Found while re-measuring `CSV::Table`'s own test suite as part of the CSV
battery survey (`docs/batteries/csv.md`) after fixing its `return-rw`/
`AT-POS` blocker: `t/1-delimiters.t` exercises exactly this pattern
(`$t.cell[0][1] = 48`) via the module's own `@.cell` (array-of-arrays)
attribute.

Pinned by `t/nested-index-assign-method-accessor.t` (array-of-arrays and
hash-of-arrays accessor cases), verified against `raku` directly.
