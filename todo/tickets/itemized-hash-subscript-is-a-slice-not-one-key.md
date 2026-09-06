# An itemized value used as a hash subscript is sliced, not taken as one key

Split out 2026-09-06 from
`todo/tickets/object-hash-key-lost-when-pair-value-is-a-container.md`, whose
other two faces are fixed in `news/2026-09/object-hash-key-survives-container-value.md`.
This one is a different mechanism — the subscript, not the Pair or the hash
assignment — so it kept its own file. Re-measured against raku v2026.07 at the
same time, including the read side, which the original note did not record.

## Repro — the WRITE side

```raku
my $s = $(1, 2);
my %c;
%c{$s} = "x";
say %c.raku;
# raku:  {"1 2" => "x"}
# mutsu: {"2" => "x"}
```

mutsu flattens `$s` into a two-element slice subscript and assigns to each in
turn, so the last one wins; raku takes the itemized value as **one** key.
Itemization is exactly what distinguishes the two — raku slices the
*non*-itemized `%c{(1, 2)}`, and dies on the object-hash form for that reason.

## And the READ side, which is wrong the same way

```raku
my %c;
%c{"1 2"} = "x";
my $s = $(1, 2);
say %c{$s}.raku;
# raku:  "x"
# mutsu: Any
```

## On a key-constrained hash it shows up as a type error

```raku
my $s = $(1, 2);
my Any:D %c{List:D};
%c{$s} = 'x';
# raku:  (my Any:D %{List:D} = (1, 2) => "x")
# mutsu: Type check failed for an element of %c; expected List:D but got Int (2)
```

The `Int (2)` in that message is the tell: the subscript had already been
flattened to the list's *last element* before the key-type check ran.

## Where to look

The flattening happens before `exec_index_assign_expr_named_op_inner`
(`src/vm/vm_var_assign_index_named.rs`) — that function's single-key key-type
check receives an `idx` that is already `Int(2)`. Trace back through
`exec_index_assign_expr_named_op_seeded_inner` /
`exec_index_assign_expr_named_op_seeded` (`src/vm/vm_var_assign_element.rs`) to
whichever step decides a list-shaped subscript is a slice, and make that
decision consult `Value::deitemize_element`'s notion of itemization (the same
one `runtime::utils::value_is_itemized_container` states) rather than treating
every list-shaped index as a slice.

## Neighbourhood to check when fixing

The read side and the write side must move together; `$[1, 2]` (itemized Array)
and `$(%h)` (itemized Hash) as subscripts; a *non*-itemized `%c{(1, 2)}`, which
must keep slicing; the positional twin `@a[$(1, 2)]`; a slice whose elements
come from a variable (`my @k = <a b>; %c{@k}`), which must keep slicing; and
`%c{$s}:exists` / `:delete`, which take the same index path.
