# A hyper over a Bag or Mix drops the weights

```raku
my $b = <a a b>.Bag;
say ($b>>.Str).raku;      # raku: ("b"=>1,"a"=>2).Bag   mutsu: ("a"=>0,"b"=>0).Bag

my $m = (a => 1.5).Mix;
say ($m>>.Str).raku;      # raku: ("a"=>1).Mix          mutsu: mix()
```

The keys survive but every weight comes back 0, and a Mix whose weights all
collapse to 0 renders as the empty `mix()` because a zero-weight element is not
in the Mix at all. The `Set` twin is fine (membership is not a weight), and so is
the `Hash` branch.

This is **not** an itemization problem — the plain form above is already wrong,
and the itemized form behaves identically once the itemization is stripped (see
[news](../../news/2026-08/hyper-on-an-itemized-hash.md)). It is the QuantHash
result-rebuilding tail of `exec_hyper_method_call_op`
(`src/vm/vm_hyper_method_ops.rs`, the `match target.view()` with the `Mix`/`Bag`
arms after the Hash writeback): it pairs `items` with `results` and reads the
weight off the *item*, which by then is the element the hyper mapped rather than
the `key => weight` Pair the rebuild expects, so `quanthash_elem_entry` yields no
weight and the entry lands at 0.

Note what the correct answer is, because it is not "map the weights": raku maps
the **elements** and keeps each one's original weight — `<a a b>.Bag>>.Str` is
still `a => 2, b => 1` because `"a".Str` is `"a"`. A method that changes the
element merges weights the way `Bag` composition does.

Worth pinning both the plain and the itemized spelling in
`t/hyper-itemized-hash.t` (which currently asserts only the `Set` case) once
fixed, plus a method that actually rewrites the element (`>>.uc`) so the
merge behaviour is covered.
