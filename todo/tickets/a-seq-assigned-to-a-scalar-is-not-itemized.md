# A `Seq` assigned to a `$` scalar is not itemized

Found 2026-09-06 while fixing
`todo/tickets/zip-chain-with-a-comma-list-left-operand-fails-to-parse.md`
(`news/2026-09/list-infix-chains-across-a-comma-list.md`). Independent of it and
of `Z` entirely — the shortest repro involves no meta-op at all.

## Repro

```raku
my $s = (1, 2).Seq;
say $s.raku;
# raku:  $((1, 2).Seq)
# mutsu: (1, 2).Seq
```

## Narrowed — it is the `Seq`, not the assignment

A `$`-scalar assignment itemizes every other list-shaped value correctly, so the
store path is fine and only `Seq` slips through:

| Program | raku | mutsu |
|---|---|---|
| `my $s = (1, 2); $s.raku` | `$(1, 2)` | `$(1, 2)` — correct |
| `my $s = [1, 2]; $s.raku` | `$[1, 2]` | check when fixing |
| **`my $s = (1, 2).Seq; $s.raku`** | `$((1, 2).Seq)` | **`(1, 2).Seq`** |
| **`my $s = zip((1,2),(3,4)); $s.raku`** | `$(((1, 3), (2, 4)).Seq)` | **no `$(`** |
| **`my $s = (1, 2 Z <a b>); $s.raku`** | `$(((1, "a"), (2, "b")).Seq)` | **no `$(`** |

The last two are just `Seq`-producing expressions; they are listed because that
is how this was noticed.

## Scope

The visible damage is limited to `.raku` (and anything that reads the
itemization off the stored value). `.elems`, element access and iteration are
unaffected in the cases measured — a `Seq` in a `$` behaves as one item for
those already. Check `.VAR.^name` (raku says `Scalar`), `.item`, and whether the
value flattens in list context (`my $s = (1,2).Seq; my @a = $s, 3` must be two
elements) before deciding how deep the fix has to go.

## Where to look

The `$`-scalar store path's itemization decision — `itemize_scalar_store` in
`src/vm/vm_var_assign_set_local.rs` is the hook the `@`/`%` sides use, and the
`Seq` view is evidently not reaching whatever marks a stored aggregate as
itemized. Compare with how `ValueView::Array(_, ArrayKind::List)` is handled at
the same point.

## Neighbourhood to check when fixing

`my $s := (1,2).Seq` (a bind, which must NOT itemize); `$s = (1,2).Seq` as a
plain assignment to an existing scalar; a `Seq` returned from a sub and assigned;
`my $s = lazy gather { ... }` (which must stay lazy — itemization must not force
it); and `.raku` round-tripping (`$((1, 2).Seq).raku.EVAL`).
