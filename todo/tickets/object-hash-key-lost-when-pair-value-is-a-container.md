# An object hash loses its typed key when the Pair's *value* came from a container read

Measured 2026-09-06 while re-triaging
`todo/deep/config-toml-battery-core-blockers.md`. This is the whole remaining
blocker for `Crane`'s `t/flatten.rakutest` and `t/list.rakutest`, and it needs
no module to reproduce.

## Minimal repro

```raku
my %h = :path($(1, 2)), :value(9);
my $p = (%h<path> => %h<value>);      # NB: the VALUE also comes from a hash read
my Any:D %t{List:D} = ($p,);
say %t.raku;
```

- raku: `(my Any:D %{List:D} = (1, 2) => 9)`
- mutsu: `Type check failed for an element of %t; expected List:D but got Str ("1 2")`

The **value** side is the trigger, not the key:

```raku
my $p = (%h<path> => 9);              # literal value -> works
my $p = ($(1, 2) => %h<value>);       # literal key, hash-read value -> FAILS
```

`key => $var` deliberately captures the RHS as a write-through `ContainerRef`
(`pop_pair_operands_capturing`, `src/vm/vm_mixin_does_ops.rs`), which is correct
for `$pair.value = X`. Somewhere between there and
`coerce_typed_container_assignment` (`src/vm/vm_var_assign_typed.rs:270`), the
Pair list is converted to a `Hash` whose `original_keys` map does **not** record
the non-`Str` key object when the value is that `ContainerRef`. The object-hash
key check then falls back to `try_reconstruct_typed_key(str_key, "List:D")`,
which hands it `Str("1 2")` and the assignment dies.

The candidate conversion site is `pair_list_to_hash`-style code in
`src/runtime/utils/coerce_containers.rs` (the `ValuePair(k, v)` arms around
lines 137 and 180, which do register `original_keys`) — but the failing path
evidently reaches `coerce_typed_container_assignment` with the hash *already*
built somewhere else, so the first job is to find which conversion actually runs
(a `rust-gdb` breakpoint on `set_hash_original_keys` will name it in one run).

## Secondary divergence found alongside it

mutsu keeps a Pair key **itemized**; raku decontainerizes it:

```raku
my $s = $(1, 2);
say ($s => "x").raku;        # raku: (1, 2) => "x"   mutsu: $(1, 2) => "x"
say ($s => "x").key.VAR.^name;  # raku: List (mutsu also reports List via .VAR)
```

Rakudo's `infix:<< => >>` binds the key as a plain `Mu $key`, so the Scalar
wrapper is gone; the *value* keeps its container in both (raku reports
`Scalar` for `("k" => $s).value.VAR`). Fixing the key side would make
`%t.raku` print `(1, 2) => 9` like raku and may or may not be the same bug as
above — measure before assuming it is.

## Also measured, not fixed

An **itemized** value used as a hash subscript must be ONE key, not a slice:

```raku
my $s = $(1, 2);
my Any:D %c{List:D};
%c{$s} = 'x';
# raku:  (my Any:D %{List:D} = (1, 2) => "x")
# mutsu: Type check failed for an element of %c; expected List:D but got Int (2)
```

mutsu flattens `$s` into a two-element slice subscript. (raku dies on the
*non*-itemized `%c{(1, 2)}` for exactly that reason, so the itemization is what
distinguishes the two, and mutsu ignores it here.)

## Why it matters

`Crane::Flatten` is one line — `my Any:D %tree{List:D} = Crane::List.list($container, :@path).map({ .<path> => .<value> })` —
and it is the exact shape above: a list of Pairs whose key and value both come
out of a hash read. `Config::TOML` does not use `Crane.flatten`, so this blocks
2 of `Crane`'s 15 upstream files, not the TOML half.
