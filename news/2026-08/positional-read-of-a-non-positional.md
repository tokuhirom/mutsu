# A positional read of a non-Positional value follows the one-element rule

`:exists` and the value adverbs learned to carry the subscript's bracket to the
runtime ([subscript kind on the exists opcode](../2026-07/subscript-kind-on-the-exists-opcode.md)),
so `(my $s = <a b>.Set)[0]:exists` correctly answered `True`. The plain *read* of
the same subscript did not, and answered the associative question instead:

```raku
my $s = <a b>.Set;
say $s[0];       # raku: Set(a b)   was: False   (the membership of the key 0)
say $s[1];       # raku: Failure, X::OutOfRange  was: False

my $c = { a => 1 };
say $c[1];       # raku: the same Failure        was: Nil
say $c.AT-POS(0);# raku: {a => 1}                was: "No such method 'AT-POS' ..."
```

raku reads `[...]` on a value that does not do `Positional` through
`Any.AT-POS`: the value is a one-element list holding itself, so index 0 is the
value and every other index is an `X::OutOfRange` Failure. `Hash`, `Set`, `Bag`
and `Mix` all do `Associative` but not `Positional`, so the rule applies to them
— while `$s{0}` stays the membership of the key `0`, which mutsu already got
right. The Hash case for index 0 happened to come out right by another route,
which is why only the quanthash shape was visibly wrong.

## What changed

The read opcode already carried the bracket: `OpCode::Index { is_positional }`
has had the flag all along, and `exec_index_op_with_positional`
(`src/vm/vm_var_index_ops.rs`) already consulted it for a `Hash` target with an
`Int` index. What was missing was the general rule, ahead of the per-container
arms:

- A `[...]` read of any value for which `is_one_element_under_positional_subscript`
  holds now answers the one-element rule — index 0 is the value, anything else
  the Failure — and an index list or Range reads each of its elements that way,
  so a slice keeps its shape. This arm precedes the Hash/Set/Bag/Mix arms, which
  keep answering `{...}`.
- The value comes back decontainerized, as `Any.AT-POS` hands it over:
  `(my $c = {a => 1})[0]` reads as `{:a(1)}`, not the itemized `${:a(1)}` the `$`
  holds. A hash carries its itemization as a flag on the value rather than a
  `Scalar` wrapper, so both axes are stripped.
- `[*]` is deliberately *not* the one-element rule. It asks for every element of
  the value's own list, which for an Associative is its pairs: `{a => 1}[*]` is
  `(:a(1),)` and `<a b>.Set[*]` is `(:a, :b)`, where mutsu previously answered
  the hash's values and `(True, True)`. It gets its own arm, also decontainerized
  first so a hash held in a `$` still lists its pairs instead of counting as the
  single item its itemization makes it in list context.
- The native `AT-POS` (`src/builtins/methods_narg/dispatch_1arg.rs`) gained the
  tail arm its `EXISTS-POS` sibling already had, so `%h.AT-POS(0)` is the hash
  and `%h.AT-POS(1)` the Failure, rather than a missing-method error.
- `Interpreter::make_scalar_index_out_of_range_failure` now delegates to the
  existing `RuntimeError::out_of_range_failure` instead of rebuilding the same
  attribute map by hand.

`t/positional-read-of-a-non-positional.t` pins 29 assertions across hashes with
both sigils, sets, bags, mixes and plain scalars, the `{...}` spellings that must
*not* change, the `[*]` list rule, a range slice, and the `AT-POS`/`EXISTS-POS`
pair. Every one of them also passes unmodified under rakudo.

One case in the family is left open and recorded as
`todo/tickets/string-index-under-a-positional-subscript-is-a-key-lookup.md`: a
*string* index under `[...]` (`$h["a"]`) is still read as a key, where raku
coerces the index with `.Int` and dies with `X::Str::Numeric`. It is filed
separately because its blast radius is different in kind — the numeric arms only
changed answers that were already wrong, while that one turns a currently-working
spelling into an exception, so it needs a local batteries-gate run first.
