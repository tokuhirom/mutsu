# A mutable QuantHash's `.pairs` value only writes back when the pair is a `for` loop's topic

## Symptom

`BagHash`/`MixHash`/`SetHash` weights are mutable through the `Pair` their
`.pairs` hands out. mutsu implements that, but only for the shape where the
pair is a `for` loop's topic — bind the same pair to a variable and the write
is lost:

```
$ raku  -e 'my $b = BagHash.new("a" xx 5); for $b.pairs { .value = 9 }; say $b<a>'   # 9
$ mutsu -e 'my $b = BagHash.new("a" xx 5); for $b.pairs { .value = 9 }; say $b<a>'   # 9   ✓

$ raku  -e 'my $b = BagHash.new("a" xx 5); my $p = $b.pairs[0]; $p.value = 9; say $b<a>'   # 9
$ mutsu -e 'my $b = BagHash.new("a" xx 5); my $p = $b.pairs[0]; $p.value = 9; say $b<a>'   # 5   ✗
```

`.value--` diverges the same way (`raku: 4`, `mutsu: 5`), which is how this was
found: `t/for-pairs-value-quanthash-writeback.t`'s first block uses a *literal*
Pair (`my $p = a => 5; $p.value--`) to test the `.value--` **parse**, and that
literal shape is itself a divergence (raku rejects it — see
`pair-value-assign-does-not-enforce-immutable-value.md`). Rewriting that block
onto a real BagHash, which is what it should have used, exposes this gap.

## Root cause

The QuantHash weight writeback in `assign_method_lvalue_with_values`
(`src/runtime/methods_mut_method_lvalue.rs`) is keyed on `self.topic_source_var`:

```rust
if let Some(source) = self.topic_source_var.clone()
    && matches!(self.env.get(&source).map(Value::view),
                Some(ValueView::Bag(_, true) | ValueView::Mix(_, true) | ValueView::Set(_, true)))
{
    self.quanthash_set_weight_elem(&code, &source, &key_elem, &value)?;
    return Ok(value);
}
```

`topic_source_var` is set by the `for` loop (`exec_for_loop_body`) and names the
container being iterated. Outside a loop it is `None`, so nothing identifies
which QuantHash the pair came from and the write falls through to the
standalone-pair compensator, which rebinds `$p` and leaves `$b` alone.

So the writeback is not attached to the **pair**; it is attached to the
**loop**. The pair `.pairs` hands out carries no back-reference to its source
container.

## Why this is not a one-liner

Fixing it properly means the `Pair` a mutable QuantHash's `.pairs` produces has
to know its source, the way ADR-0036's element containers know theirs — which
is exactly the "route `.pairs` at the producer" work that ADR-0036 slice 3
landed on 2026-09-01
(`news/2026-09/pairs-hands-out-element-containers.md`) -- for a plain mutable
Array/Hash only.
A QuantHash weight is not a stored element container, though, and
`.value = 0` *removes* the key, so it cannot simply become a `ContainerRef`
either — ADR-0036 §5 Q2 records the decision to keep the weight on its own arm.
What is missing is a narrower carrier: a weight-flavoured back-reference on the
Pair, checked by the same `.value` lvalue arm that checks `topic_source_var`
today.

## Interaction with the pending read-only guard

`todo/tickets/pair-value-assign-does-not-enforce-immutable-value.md` will delete
the standalone-pair compensator that currently swallows this write. The guard is
designed to fire **only for an immutable scalar pair value**, and a BagHash
weight is an `Int`, so once it lands this shape turns from a silent no-op into a
spurious `X::Assignment::RO`. **Land this ticket's fix first, or make the guard
skip a pair whose key exists in a mutable QuantHash named by any live binding.**
The former is correct; the latter is another scan.

## Also found

`MixHash.new-from-pairs(a => 2.5).pairs[0]` returns something mutsu answers
`.value` on but raku does not (`No such method 'value' for invocant of type
'Any'`) — raku has no `new-from-pairs` on `MixHash`, so that is a mutsu-only
constructor rather than a divergence in `.pairs`. Not part of this ticket, but
worth not mistaking for one.

## Repro to pin when fixed

```raku
{
    my $b = BagHash.new("a" xx 5);
    my $p = $b.pairs[0];
    $p.value = 9;
    is $b<a>, 9, 'a bound QuantHash pair writes its weight back';
    $p.value--;
    is $b<a>, 8, 'and so does .value-- on it';
}
{
    my $b = BagHash.new("a" xx 5);
    my $p = $b.pairs[0];
    $p.value = 0;
    nok $b<a>:exists, 'weight 0 removes the key, as it does through the loop form';
}
```
