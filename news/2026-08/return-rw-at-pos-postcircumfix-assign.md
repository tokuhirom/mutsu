# `$obj[$i] = v` now writes through a `return-rw`/`is rw` `AT-POS`/`AT-KEY` when no `ASSIGN-POS`/`ASSIGN-KEY` is declared

A user `Positional`/`Associative` class with no explicit `ASSIGN-POS`/
`ASSIGN-KEY` but an `AT-POS`/`AT-KEY` whose body returns an indexed attribute
element (`@!attr[$i]`, `return-rw @!attr[$i]`, or the `%`-sigil equivalent)
previously had its postcircumfix `$obj[$i] = v` / `$obj{$k} = v` silently
dropped:

```raku
class ArrayOneBased does Positional does Iterable {
    has @.arr;
    method AT-POS($i) {
        return-rw @!arr[$i];
    }
}
my $a = ArrayOneBased.new;
$a[1] = 0;
say $a.raku;   # was: ArrayOneBased.new(arr => []) -- now: ArrayOneBased.new(arr => [(Any), 0])
```

Real Raku's default `Positional`/`Associative` protocol falls back to calling
`AT-POS`/`AT-KEY` and, if it returned a writable container (`is rw` or
`return-rw`), assigns through that container when no `ASSIGN-POS`/
`ASSIGN-KEY` is declared. mutsu had no such fallback: the postcircumfix
assign path (`src/vm/vm_var_assign_index_named.rs`) unconditionally dropped
the write for any "plain user object" class, because that same code path is
also used for the compiler's mutate-then-write-back lowering of
`$b[0].a = 11` (where dropping is correct — the element was already mutated
in place through a shared instance).

Fixed by reusing the same body-shape detection
(`rw_method_indexed_attr_target`) that already backs the explicit
`.AT-POS(i) = v` method-call lvalue form
(`src/runtime/methods_mut_method_lvalue.rs`): when the class's `AT-POS`/
`AT-KEY` body matches `@!attr[$i]` / `return-rw @!attr[$i]`, the postcircumfix
assign now routes through `assign_rw_indexed_attr` instead of dropping. The
`$b[0].a = 11` write-back case is unaffected — it either doesn't match this
shape, or (when it does) rewrites the same object back into the same slot, a
no-op.

This was discovered while measuring `CSV::Table`'s test suite as part of the
CSV battery survey (`docs/batteries/csv.md`): its transitive dependency
`AlgorithmsIT::Classes::ArrayOneBased` uses exactly this pattern, and every
`CSV::Table` test that actually constructs a table object hit it during
`TWEAK`. `use CSV::Table` now loads and constructs objects cleanly; its own
suite surfaces further, unrelated gaps (delimiter/comment/save/half-matrix
formatting) not covered by this fix.

Pinned by `t/at-pos-return-rw-assign.t`. Resolves
`todo/tickets/custom-at-pos-return-rw-index-assign.md`.
