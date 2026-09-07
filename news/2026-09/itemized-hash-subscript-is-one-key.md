# An itemized value used as a hash subscript is one key, not a slice

`my $s = $(1, 2); my %c; %c{$s} = "x"` produced `{"2" => "x"}` in mutsu where
raku produces `{"1 2" => "x"}`. The itemized list was flattened into a
two-element *slice* subscript and assigned element by element, so the last one
won. The read side was wrong the same way — after `%c{"1 2"} = "x"`, `%c{$s}`
answered `Any` — and so were `:exists` and `:delete`, which take the same index
path. On a key-constrained hash the flattening surfaced as a type error whose
message named the list's last element:

```
my Any:D %c{List:D}; %c{$(1, 2)} = 'x';
# mutsu: Type check failed for an element of %c; expected List:D but got Int (2)
# raku:  (my Any:D %{List:D} = (1, 2) => "x")
```

Itemization is exactly what tells a single subscript from a slice: raku slices
the *non*-itemized `%c{(1, 2)}` and takes the itemized `%c{$s}` as one key.
mutsu already knew that rule, but only in its positional spelling — an itemized
positional subscript numifies to the itemized list's element count
(`@a[$(7,8,9)]` is index 3), and all four subscript opcodes applied that
numification unconditionally, hash brackets included. Numifying a *hash*
subscript made the key the element **count**, which is why `%c{$(1, 2)}` and
`%c{$(3, 4)}` were the same key `"2"`.

## The fix

"One subscript" means different things under `[ ]` and `{ }`, so each of the
four index paths now branches on the bracket kind rather than numifying
outright:

- `src/vm/vm_var_index_ops.rs` (read), `src/vm/vm_var_exists_ops.rs`
  (`:exists`) and `src/vm/vm_var_assign_index_named.rs` (assign) already carry
  the bracket kind (`is_positional` / `SubscriptKind`), so the numification is
  now gated on it.
- `src/vm/vm_var_delete_ops.rs` (`:delete`) does not — its opcode records only
  the variable name — so it decides from the target instead: a `%h`-shaped
  container is exactly the case that must not numify.

Under a hash subscript the itemized value is instead normalized to a `Scalar`
wrapper. That is the one shape the slice machinery does not treat as a list (it
matches `ValueView::Array` regardless of `ArrayKind`, in a dozen places), and
it is the same canonical form on all four paths, so read, assign, `:exists` and
`:delete` agree on the key — and on its `.WHICH`, which is what an object hash
keys by. The slice arm of the assign path additionally states the rule where
the slice decision is made (`is_positional || !kind.is_itemized()`).

Object hashes now work end to end for this shape:

```
my %h{Any}; my $key = $(1, 2, 3); %h{$key} = 42;
say %h.elems;                 # 1
say %h{$key};                 # 42
say (%h{$key}:exists);        # True
%h{$key}:delete; say %h.elems # 0
```

Everything the flattening legitimately covered keeps working: a non-itemized
`%c{(1, 2)}` still slices, a slice built from a variable (`my @k = <a b>;
%c{@k}`) still slices, and every positional form — `@a[$(7,8,9)]`,
`@a[my $ = ^2]`, and their `:exists` / `:delete` twins — still numifies to the
element count.

Pinned by `t/itemized-hash-subscript.t` (17 assertions, each also verified
against rakudo v2026.07).
