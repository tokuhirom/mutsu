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
- A **Range** slice of the same one-element list gets its own arm, because it
  differs from the single-index read in two ways that matter: an out-of-range
  range *throws* eagerly rather than answering a Failure (`'foo'[2..3]` is
  `X::OutOfRange` with `got => 2`, pinned by `roast/S02-types/lists.t`), and it
  must reach that verdict from the range's *start* without reifying a lazy one
  (`'foo'[2..*]`). The hash-specific "a Range subscript is a multi-key slice"
  rewrite now fires only for `{...}`, so a `[...]` Range reaches this arm.
- The native `AT-POS` (`src/builtins/methods_narg/dispatch_1arg.rs`) gained the
  tail arm its `EXISTS-POS` sibling already had, so `%h.AT-POS(0)` is the hash
  and `%h.AT-POS(1)` the Failure, rather than a missing-method error.
- `Interpreter::make_scalar_index_out_of_range_failure` now delegates to the
  existing `RuntimeError::out_of_range_failure` instead of rebuilding the same
  attribute map by hand.

## The other half: a `[...]` index is a number

The bracket decides the *index* as well as the protocol. `AT-POS` takes an
`Int`, so raku numifies a positional index and a string index is a number, never
a key:

```raku
my @a = 10, 20, 30;
say @a["1"];     # 20      (unchanged)
say @a["1.9"];   # 20      was: a "does not support associative indexing" error
say @a["x"];     # X::Str::Numeric   was: the same generic error
my $h = { a => 1, '1' => 'one' };
say $h["a"];     # X::Str::Numeric   was: 1     -- read as the key `a`
say $h["1"];     # X::OutOfRange     was: 'one' -- read as the key `1`
say $h<1>;       # 'one'   (the associative spelling still finds the key)
```

The coercion happens once, up front, so the per-container arms never see a Str
under `[...]` — the old `(Array, Str) if is_positional` arm, which parsed the
string itself and reported a failure as an associative-indexing error, is gone.

Two exclusions are load-bearing. A `Package` target is skipped because `[...]`
on a type name is not a subscript at all but a **parameterization**:
`role Doc[Str $d]` invoked as `Doc[$doc]` compiles to this same opcode, and
numifying its argument broke every string-parameterized role (caught by
`t/variable-custom-traits.t`). `Instance`/`Mixin` targets are left to their own
subscript protocol.

Three local tests asserted the old lenient behaviour — `hash(...)["a"]`,
`categorize(...)["even"]`, `gethost()["name"]` — and were rewritten to the `<...>`
spelling they meant; all three are errors under rakudo as written.

`t/positional-read-of-a-non-positional.t` pins 36 assertions across hashes with
both sigils, sets, bags, mixes and plain scalars, the `{...}` spellings that must
*not* change, the `[*]` list rule, a range slice, string-index numification, a
string-parameterized role, and the `AT-POS`/`EXISTS-POS` pair. Every one of them
also passes unmodified under rakudo.

One divergence is deliberate and left as is: mutsu answers a coercion or
out-of-range Failure where rakudo throws as soon as the Failure is bound or the
containing list is used. That is the general Failure model rather than this path,
so the tests assert what both runtimes agree on — that nothing is read.
