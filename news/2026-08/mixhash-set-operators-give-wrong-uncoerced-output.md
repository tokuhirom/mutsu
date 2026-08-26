# `Mix(...)`/`Bag(...)`/`Set(...)` are coercions, not `.new` constructors — which is why the weighted set operators looked broken on `MixHash`

`MixHash (^) MixHash` and `MixHash (+) MixHash` produced garbage — raw
`key => value` `Pair` gists mixed in with computed `key(weight)` results, and
none of the weighted arithmetic actually performed:

```raku
my ($a, $b) = MixHash(2 => 2, 4), MixHash(2 => 1.5, 3 => 2, 4);
say $a (^) $b;   # raku: MixHash(2(0.5) 3(2))
                 # mutsu: MixHash(2 => 2 2 => 1.5 3 => 2)
```

## Root cause — not the set operators at all

The set operators were fine. The **operands** were malformed: `MixHash(2 => 2, 4)`
built a MixHash whose keys were the `Pair` `2 => 2` and the `Int` `4`, each with
weight 1, instead of key `2` with weight 2 and key `4` with weight 1. The
ticket's own "plain `Mix` works correctly" evidence was an artefact of testing
the *method* spelling `(2 => 2, 4).Mix` there and the *function* spelling
`MixHash(...)` here — the function spelling was equally broken for `Mix`, `Bag`,
`BagHash`, `Set` and `SetHash`.

Raku has two distinct families here, and mutsu had collapsed them onto one
implementation:

* The **capitalised** spellings are coercions — `multi sub Mix(+@a) { @a.Mix }`.
  The arguments are slurped into a list and that list is *coerced*, so a
  positional `Pair` contributes `key => weight` and a nested QuantHash spills
  its own pairs (`Bag(Bag(1,1))` is `(1=>2).Bag`).
* The **lowercase** `set`/`bag`/`mix` are `.new`-flavoured: every element,
  `Pair`s included, stays an opaque key of weight 1 (`bag(2 => 2, 4)` really is
  `(2 => 2=>1, 4=>1).Bag`, and `bag(bag(1,1))` keeps the inner `Bag` as a key).

mutsu routed both families through `builtin_set`/`builtin_bag`/`builtin_mix`,
which implement the lowercase (`.new`) semantics — correctly for `set`/`bag`/`mix`,
wrongly for `Set`/`Bag`/`Mix`/`SetHash`/`BagHash`/`MixHash`.

## Fix

The capitalised spellings now build the `+@a` list and hand it to the very same
`builtins::quanthash_coerce::{to_set, to_bag, to_mix, to_mixhash}` builders the
`.Set`/`.Bag`/`.Mix` **methods** already use — one implementation per operation.
`set`/`bag`/`mix` keep the `.new` builders unchanged.

With well-formed operands the existing weighted `(^)`/`(+)`/`(&)`/`(-)`
implementations produce rakudo's answers with no change of their own.

Pinned by `t/numeric-coercion-gaps.t`, which checks the pair-weight reading for
all four `Mix`/`MixHash`/`Bag`/`BagHash` coercion spellings, the nested-`Bag`
spill, that lowercase `bag(...)` still keeps a `Pair` opaque, and the resulting
`MixHash (^)`/`(+)` weights.
