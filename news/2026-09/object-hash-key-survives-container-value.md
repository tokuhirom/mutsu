# An object hash keeps its typed key when the Pair's value came from a container

Two independent defects on the way from a `key => value` Pair into a
key-constrained hash. Together they were the whole remaining blocker for
`Crane`'s `t/flatten.rakutest` and `t/list.rakutest`, whose `Crane::Flatten` is
one line of exactly this shape — a list of Pairs whose key *and* value both come
out of a hash read.

```raku
my %h = :path($(1, 2)), :value(9);
my $p = (%h<path> => %h<value>);
my Any:D %t{List:D} = ($p,);
say %t.raku;
# raku:  (my Any:D %{List:D} = (1, 2) => 9)
# mutsu: Type check failed for an element of %t; expected List:D but got Str ("1 2")
```

## 1. The Pair's key was not decontainerized

Rakudo's `infix:<< => >>` binds the key as a plain `Mu $key` and the value as
`Mu \value`, so only the **value** keeps a container. mutsu handed the key
through whole, so an itemized key stayed itemized:

```raku
my $s = $(1, 2);
say ($s => "x").raku;           # raku: (1, 2) => "x"   mutsu (before): $(1, 2) => "x"
say ($s => "x").key.VAR.^name;  # raku: List            mutsu (before): Scalar
```

`pop_pair_operands_capturing` (`src/vm/vm_mixin_does_ops.rs`) now applies
`deitemize_element` to the key. The value side is untouched — its write-through
capture is what makes `$pair.value = X` update the source variable, and an
itemized value still reads back itemized.

## 2. Dereferencing a hash's bound cells threw away its metadata

The real cause of the type error, and the reason a literal value succeeded where
a container read failed. Assigning to a `%` variable, mutsu checks whether any
of the incoming hash's values is a `:=`-bound cell and, if so, snapshots them —
which is correct, assignment copies. But `resolve_hash_for_iteration`
(`src/vm/vm_var_ops.rs`) did it by building a **bare** `Value::hash(map)`,
keeping only the entries. Every other field of the `HashData` went over the
cliff with it: `original_keys` (so the object-hash key `(1, 2)` was gone, and the
key check fell back to reconstructing `Str("1 2")` from the store key),
`key_type`, `value_type`, `declared_type` and the `is default(...)` value.

Nothing about the *key* triggered it — the pair's **value** being a
`ContainerRef` from `%h<value>` is what brought this deref into play at all,
which is exactly why `($(1, 2) => 9)` worked and `($(1, 2) => %h<value>)` did
not. The deref is now done in place through copy-on-write, so only the entries
change and everything else travels along.

## Testing

`t/object-hash-key-survives-container-value.t` — 21 assertions, all measured
against raku v2026.07 first. Both halves, plus what must not move: the value
keeping its container and writing through, an itemized value staying itemized, a
plain `Array` / literal-list / scalar key unchanged, all four
literal/container-read combinations of the object-hash assignment, and the
`Int` value-type constraint, `is default(...)` value and plain-hash assignment
that shared the second defect's cliff.

## Split out, not fixed here

The third face recorded on the original ticket is a different mechanism — the
subscript rather than the Pair or the assignment — so it kept its own file,
`todo/tickets/itemized-hash-subscript-is-a-slice-not-one-key.md`: `%c{$s} = "x"`
with `$s = $(1, 2)` slices into two keys where raku uses one. Re-measuring for
that split showed the **read** side (`%c{$s}`) is wrong the same way, which the
original note had not recorded.
