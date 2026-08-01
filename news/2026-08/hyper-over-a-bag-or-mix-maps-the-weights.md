# A hyper over a Bag or Mix maps the weights

A hyper method call on a QuantHash used to lose every weight:

```raku
my $b = <a a b>.Bag;
say ($b>>.Str).raku;      # was ("a"=>0,"b"=>0).Bag   now ("a"=>2,"b"=>1).Bag
my $m = (a => 1.5).Mix;
say ($m>>.Str).raku;      # was mix()                 now ("a"=>1).Mix
```

The keys survived but every weight came back 0, and a Mix whose weights all
collapse to 0 renders as the empty `mix()` because a zero-weight element is not
in the Mix at all. The `Set` twin looked fine only because membership is not a
weight.

It was never an itemization problem — the plain form above was already wrong and
the itemized form behaved identically (see
[hyper-on-an-itemized-hash](hyper-on-an-itemized-hash.md)). It was the QuantHash
result-rebuilding tail of `exec_hyper_method_call_op`
(`src/vm/vm_hyper_method_ops.rs`): it paired the walked items with the results
and read each weight off the *item*, but by then the item was whatever the hyper
had mapped rather than the `elem => weight` Pair the rebuild expected, so
`quanthash_elem_entry` yielded no weight and the entry landed at 0.

## What the correct answer turned out to be

Not "also map the weights alongside the elements": a QuantHash hypers exactly the
way a Hash does — the method sees each **weight** and never the element at all.
`<a a b>.Bag>>.uc` is still `a => 2, b => 1`, because `.uc` is applied to the
counts `2` and `1` (`2.uc` is `"2"`, which coerces straight back to `2`), not to
the keys. The original ticket guessed the opposite (elements mapped, weights
carried along), which gives the same answer for the reported `>>.Str` symptom and
is why the two readings were indistinguishable from it; `>>.uc` settles it.

The per-type coercion mirrors Rakudo's `deepmap` candidates, which are less
uniform than one would hope and are now documented on `QuantHashHyper`:

| target    | mapper argument | weight coercion | element kept when |
| --------- | --------------- | --------------- | ----------------- |
| `Bag`     | the count       | `.Int`          | `> 0`             |
| `BagHash` | the count       | `.Int`          | `> 0`             |
| `Mix`     | the weight      | `.Int`          | non-zero          |
| `MixHash` | the weight      | `Real` (kept)   | non-zero          |
| `Set`     | `1`             | `Bool`          | truthy            |
| `SetHash` | `1`             | `Bool`          | truthy            |

The immutable `Mix` truncating to `Int` while a `MixHash` keeps the `Real` is
genuinely what `Mixy.deepmap` and `MixHash.deepmap` do upstream (rakudo issue
5057 is cited in that very code), so `(a => 1.5).Mix>>.Str` is `a => 1` while
`(a => 1.5).MixHash>>.abs` stays `a => 1.5`.

## The fix

A QuantHash target is now recognised up front: its `elem => weight` pair list is
split into parallel element and weight lists, the weights become the items the
hyper walks, and the elements are set aside to rebuild the result. Both hyper
sites (`exec_hyper_method_call_op` and `exec_hyper_method_call_dynamic_op`) had
their own copy of the old rebuild; they now share `split_quanthash_items` and
`rebuild_quanthash_hyper`, which is where the table above lives.

One coercion detail needed its own helper: the mapped weight is `.Int`-ed with
`hyper_weight_as_int`, which numifies first and truncates, rather than with
`to_int`, which parses a string as an integer. `(a => 1.5).Mix>>.Str` produces
the weight `"1.5"`, and `to_int` read that as `0` — dropping the element instead
of weighting it `1`.

Pinned in `t/hyper-itemized-hash.t`, which previously asserted only the `Set`
case and carried a comment recording the Bag/Mix breakage. The new assertions
cover the plain and itemized spellings, a method that changes the weight
(`>>.succ` / `>>.pred`, including a count that falls to 0 and so leaves the Bag),
the immutable-vs-mutable Mix split, `Set`/`SetHash` truthiness, and a Bag of
non-`Str` elements; all of them pass unmodified under rakudo.
