# A subscript carries its bracket to the runtime

`$c[0]` and `$c{0}` are different questions, and until now mutsu could not tell
them apart once compiled. The `:exists` opcode and the `:kv` / `:p` / `:k` / `:v`
adverb call both received a target and an index and nothing else, so the runtime
had to guess the subscript's kind from the index's runtime type — a string index
meant associative, anything else positional. That guess is wrong for every
target that is `Associative` but not `Positional`:

```raku
my $c = { a => 1 };
say $c[0]:exists;   # raku: True   mutsu was: False
say $c{0}:exists;   # raku: False  (a key lookup for the key "0")
```

raku reads `$c[0]` through `Any.EXISTS-POS`: a value that does not do
`Positional` is a one-element list holding itself, so index 0 exists and nothing
else does. The one-element rule already worked for plain scalars
([the scalar `:exists` slice](scalar-positional-exists.md)), but
`Value::is_one_element_scalar` had to exclude `Hash`, `Set`, `Bag` and `Mix`
precisely because applying it blindly would have broken `%h{0}` — the numeric-key
lookup mutsu got right.

The fix is to stop guessing. `SubscriptKind` (`Unknown` / `Positional` /
`Associative`) now rides in bits 8-9 of the `ExistsIndexAdv` /
`ExistsIndexNamedAdv` flag word, set by the compiler from the `is_positional`
flag the parser already recorded on `Expr::Index`. The value adverbs, which
compile to a `__mutsu_subscript_adverb` call rather than an opcode, carry the
same information as a marker argument alongside the call's other tagged extras
(`__adverb_cond__`, the `:delete` pair), so the existing argument slots keep
their indices.

With the bracket in hand:

- `Interpreter::call_exists_pos` picks `EXISTS-POS` or `EXISTS-KEY` from the
  syntax instead of the index's type. The old heuristic survives only for
  `SubscriptKind::Unknown` — a zen slice, or a target the compiler did not
  recognise as an `Index`.
- When neither method is found, a positional subscript falls back to
  `Any.EXISTS-POS` rather than answering `False`. This is what makes
  `Foo.new[0]:exists` True for a class that declares neither `EXISTS-POS` nor
  `AT-POS`, matching rakudo.
- The native `EXISTS-POS` method widened from `is_one_element_scalar` to the new
  `is_one_element_under_positional_subscript`, so `%h.EXISTS-POS(1)` is `False`
  however many keys the hash holds instead of raising "No such method".
- `builtin_subscript_adverb` coerces a positionally-subscripted non-`Positional`
  target to a one-element array up front, the same way it already coerced a
  Range or a Seq, so the Array arm owns the key/value logic. `$i[0]:kv` is
  `(0, 5)`, `$i[0]:p` is `0 => 5`, and `$i[1]:v` is `()` — all of which returned
  `Nil` before. The element is decontainerized on the way in, as `Any.AT-POS`
  is: `(my $c = {a => 1})[0]:v` reads back as `{:a(1)}`, not the itemized
  `${:a(1)}` the `$` variable holds.

`t/subscript-kind-positional.t` pins 31 assertions across hashes, sets, bags,
mixes, plain scalars and instances; every one of them also passes unmodified
under rakudo.

Two adjacent gaps surfaced while doing this and are recorded as tickets rather
than folded in: the plain *read* path is still kind-blind (`(my $s = <a b>.Set)[0]`
answers `False`, reading `[0]` as the key `0`), and an *associative* subscript on
a non-`Associative` value answers `Nil` where raku answers `()`. Both need the
same treatment applied to a different set of opcodes.
