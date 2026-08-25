# `BagHash.add` / `BagHash.remove`

`BagHash` supported every mutation the subscript store can express (`$n<c>++`,
`$n<b> -= 1`, `$n{'a'} = 0`), but the two *named* per-key count mutators the
type also has -- `add` and `remove`, both documented in
`raku-doc/doc/Type/BagHash.rakudoc` under "Updating BagHash Objects" -- threw
`No such method 'add'/'remove' for invocant of type 'BagHash'`. They are
implemented now.

## The semantics, established against the reference implementation

The original ticket assumed `add`/`remove` were `Baggy` role methods and that
`MixHash` should therefore gain them too. Probing rakudo says otherwise, and
that turned out to matter:

```
BagHash: add=True  remove=True    # :(BagHash:D $:: \to-add, *%_ --> Nil)
MixHash: add=False remove=False
SetHash: add=False remove=False
Bag/Mix: add=False remove=False
```

`BagHash.^can('add')[0].package.^name` is `BagHash` -- rakudo declares both
methods on the class itself, not on the role. So `MixHash`, `SetHash`, `Bag`
and `Mix` correctly have no such method, and mutsu keeps answering "No such
method" for all four rather than inventing a fractional-weight `MixHash.add`.

The rest of the contract, also measured rather than guessed:

* Exactly **one** positional argument (`\to-add`); any other arity is
  `Too few/many positionals passed; expected 2 arguments but got N`.
* That argument is iterated **one level**. A `Str`, an `Int` or a `Pair` is a
  single element -- a `Pair` is not `Iterable`, so `$b.add('c' => 3)` makes the
  *pair* a key with count 1, it does not add `c` with weight 3. A
  `List`/`Array`/`Seq`/`Range` yields its elements, and a `Hash`/`Set`/`Bag`/
  `Mix` yields its `key => weight` pairs (so `$b.add(bag(<u v v>))` stores the
  two pairs as keys, it does not merge the bags). There is no deep flattening:
  in `('a', ('b', 'b'))` the inner list is one element of its own.
* Each yielded element moves its own count by exactly `+1` / `-1`, so a
  duplicated element moves twice.
* A count landing at or below zero drops the key entirely -- the same rule the
  subscript store already enforced for `$b<k> = 0`. `remove` of an absent key
  is a no-op and never stores a negative count.
* Both return `Nil`.

## Implementation

`src/vm/vm_baghash_mutators.rs` holds the one implementation (the argument
iteration is `runtime::utils::value_to_list`, which is exactly rakudo's
one-level iteration, and the keys go through the existing WHICH-keyed
`quanthash_elem_entry` / `record_quanthash_original` helpers, so `add`/`remove`
share the element-identity rules with every other QuantHash store site).

The counts are adjusted **in place through the bag's shared `Gc` node**, the
same mechanism `$b<k>++` uses, rather than by rebuilding the bag and writing it
back under the invocant's variable name. That is what makes aliasing behave:
`my $b = $a; $a.add('x')` is visible through `$b`, and a mutation through a sub
parameter is visible at the caller -- exactly as in rakudo, and unlike the
existing `ASSIGN-KEY` writeback path, which severs aliases. It also means an
invocant with no variable name to write back through works: `$obj.bag.add('q')`
and `@bags[0].add('p')` both mutate the real bag.

Two VM entry points call that helper: the Tier-A mutable-method path
(`vm/vm_call_method_mut_ops.rs`) for the named-variable invocant, and the
plain `CallMethod` path (`vm/vm_call_method_ops.rs`) for a value invocant with
no name -- mirroring how the array `shift`/`pop` fast path is already split
across the two. Neither adds a `runtime/methods.rs` slow-path handler.

The named-variable path additionally re-seats the (already mutated) value in
both halves of the dual store. That is not what makes the mutation visible; it
prevents a later `locals`<->`env` sync from resurrecting a stale snapshot,
which `my %b is BagHash` reproduced deterministically -- a `.add` followed by
any intervening statement and then a `.remove` lost the added key.

`t/baghash-add-remove.t` pins all of the above (44 assertions, passing under
both `raku` and `mutsu`), including the four types that must *not* get the
methods.
