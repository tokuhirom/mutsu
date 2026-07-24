# Set/Bag/Mix element identity is `.WHICH`-based (PLAN 8.10, QuantHash half)

The second half of the `.WHICH`-keying campaign (the object-hash half landed in
PR #5333): the QuantHash backing stores (`SetData.elements`, `BagData.counts`,
`MixData.weights`) now key elements by `value_which_key` instead of their
stringification, with the element objects recorded in `original_keys`. `1`,
`"1"`, `1.0` and the allomorph `<1>` are four distinct Set elements
(previously one), two instances of the same class are distinct elements, and
membership (`∈`, subscripts, `:exists`, `:delete`) is element identity
(`===`) — `<1> ∈ (1,).Set` is now False, matching Rakudo.

Design points:

- **Storage invariant.** Store key = `value_which_key(elem)`; `original_keys`
  records the element object for every non-`Str` element. A plain `Str`
  element roundtrips losslessly through its `"Str|<s>"` key, so it is not
  recorded (memory) — `typed_key` strips the prefix on decode. The
  construction helpers live in `runtime/utils/quanthash_keys.rs`
  (`quanthash_elem_entry` / `record_quanthash_original` /
  `quanthash_insert_set` / `quanthash_typed_pair`).
- **`value_which_key` gained an allomorph arm** (`IntStr|Int|1|Str|1`, both
  halves — the role-name fold alone collapsed `IntStr.new(1, "one")` with
  `IntStr.new(1, "1")`), and the `Pair`/`ValuePair` arms were unified
  (`Pair|Str|x|Int|1` for both representations of `x => 1` — they are the
  same identity).
- **Set operators thread `original_keys`.** The union/intersection/difference/
  symmetric-difference/multiply/addition coercion helpers (`ops_set.rs`,
  `vm_set_ops.rs`, `utils/set_ops.rs`, `utils/set_coerce.rs`) take an
  originals accumulator and the results are built with the typed
  constructors — previously they dropped the recorded objects entirely
  (a silent degrade that would have become corruption under WHICH keys).
- **~30 consumer surfaces converted** from raw store keys to `typed_key`
  decode: subscript reads/`:exists`/`:delete`, `AT/EXISTS/ASSIGN/DELETE-KEY`,
  `.pick`/`.roll`/`.grab`/`.grabpairs`/`.pickpairs` (including the weighted
  samplers, which existed in two copies), `.pairs`/`.kv`/`.antipairs`/`.kxxv`,
  `.invert`, `.fmt` (1- and 2-arg), `.hash`/`.Hash`/`.Map`/`.Capture`,
  `value_to_list` (both copies), `minpairs`/`maxpairs`, hyper method
  rebuilds (`$s>>.&{...}`), signature smart-match named-args, `Str`/`gist`/
  `raku` rendering, and the `for $qh.pairs/.values/.kv` writeback paths
  (`quanthash_set_weight_elem`).
- **Mutations preserve identity**: subscript assignment (named + typed-autoviv
  `my SetHash $sh; $sh<k> = v`), `++`/`--` (post/prefix), `SetHash.set/unset`,
  the `ASSIGN-KEY`/`DELETE-KEY` method forms (which previously dropped
  `original_keys` wholesale) and grab/grabpairs all record/drop the element
  object alongside the store key.
- **Parameterized QuantHash re-key** (`my %bh is BagHash[Int] = 1,2,3`): the
  trait path type-checks the *decoded elements* (the old string-reconstruct
  logic misread WHICH keys as Str elements) and a coercion parameter
  (`Int()`) re-keys the store under the coerced object's `.WHICH`. The
  now-unused `try_coerce_str_to_type` was removed.
- **Self-referential QuantHashes render without recursing** (`%sh ,= 1`,
  rakudo#4678): `Str`/`gist`/`raku` guard on the store pointer
  (`with_quanthash_render_guard`) and print `...` for a cycle — previously a
  stack overflow (abort) since rendering now decodes element objects.
- **Serde**: `SerValue::Set/Bag/Mix` carry `original_keys`;
  `CACHE_FORMAT_VERSION` bumped to 7.
- **eqv simplification**: the Set arm's allomorph-kind workaround is gone —
  comparing the WHICH-key sets IS element-identity comparison.

Local pins that asserted the old stringly behavior were updated against the
reference raku (`bag(1,1,2){'1'}` is 0 in raku — the Str key misses the Int
elements; `$m<2>` on Int elements is the IntStr allomorph and misses too), and
`t/pair-new-container-alias.t`-style oracle checks were used throughout. New
pin: `t/set-bag-mix-which-keys.t` (25 assertions, verified 25/25 under raku).
Roast: the full QuantHash family (`set.t`, `sethash.t`, `bag.t`, `baghash.t`,
`baggy.t`, `mix.t`, `mixhash.t`, the three `*-iterator.t`, and the ten
`S03-operators/set_*.t`) passes.
