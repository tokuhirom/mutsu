# A positional *read* of an Associative container is still a key lookup

`:exists` and the value adverbs now carry the subscript's bracket to the runtime
([news](../../news/2026-07/subscript-kind-on-the-exists-opcode.md)), so
`(my $s = <a b>.Set)[0]:exists` correctly answers `True` — the Set is a
one-element list holding itself under `[...]`. The plain read of the same
subscript does not:

```raku
my $s = <a b>.Set;
say ($s[0]).raku;   # raku: Set.new("b","a")   mutsu: Bool::False
say ($s[1]).raku;   # raku: Failure (X::OutOfRange, "Index out of range. Is: 1, should be in 0..0")

my $c = { a => 1 };
say ($c[1]).raku;   # raku: the same Failure   mutsu: Nil
say $c.AT-POS(0);   # raku: {a => 1}           mutsu: "No such method 'AT-POS' for invocant of type 'Hash'"
```

mutsu reads `$s[0]` as `$s{0}`, so a Set/Bag/Mix answers with the membership of
the key `0` and a Hash answers `Nil` for a missing key. The Hash case for index 0
happens to come out right by another route, which is why only the Set/Bag/Mix
shape is visibly wrong.

The cause is the same missing distinction, on a different set of opcodes: the
read path is `Compiler::compile_expr_index` (`src/compiler/expr_data.rs`), which
already *receives* `is_positional` but emits `Index` / `IndexAutovivify*` /
`IndexAutovivifyLazy*` without it, and those opcodes then dispatch on the
target's and index's runtime types in `src/vm/vm_var_index_ops.rs`. Threading
`SubscriptKind` (`src/opcode.rs`) through them is the same change already made
for `ExistsIndexAdv`, but across more opcodes and with the autovivification
paths — which pick Array-vs-Hash for a missing intermediate container from the
same flag — in the blast radius.

Two smaller pieces belong with it:

- The native `AT-POS` (`src/builtins/methods_narg/dispatch_1arg.rs`) should get
  the `is_one_element_under_positional_subscript` tail arm its `EXISTS-POS`
  sibling already has, so `%h.AT-POS(0)` is the hash rather than a missing-method
  error.
- An out-of-range positional read of a one-element value should return the
  `X::OutOfRange` Failure (`make_scalar_index_out_of_range_failure` in
  `src/vm/vm_var_index_ops.rs` already builds it for `5[1]`), not `Nil`.
