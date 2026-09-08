# A subscript store into a scalar follows the Positional/Associative protocol

A subscript store into a `$` that already holds a **defined** value is governed
by whether that value does the subscript's role. Rakudo refuses the ones that do
not:

```
my $s = (1,2,3).Seq; $s<k> = 5   X::AdHoc  "Type Seq does not support associative indexing."
my $s = [1,2,3];     $s<k> = 5   X::AdHoc  "Type Array does not support associative indexing."
my $s = 1..3;        $s<k> = 5   X::AdHoc  "Type Range does not support associative indexing."
my $s = 42;          $s<k> = 5   X::AdHoc  "Type Int does not support associative indexing."
my $s = 42;          $s[0] = 5   X::Assignment::RO  "Cannot modify an immutable Int (42)"
my $s = "str";       $s[0] = 5   X::Assignment::RO  "Cannot modify an immutable Str (str)"
```

mutsu did none of that. An associative store replaced the value outright with a
fresh `Hash` — `my $s = 42; $s<k> = 5` left `$s` as `{k => 5}`, and so did a
`Seq` and a `Str` — while an `Array` answered "Index out of bounds" and a
positional store into an `Int` or `Str` replaced it with `[5]`.

This is section E of the immutable-lvalue survey
([#7556](https://github.com/tokuhirom/mutsu/issues/7556)), which had it filed
under "not an immutability row at all — it belongs with whatever enforces the
Positional/Associative protocol per type". That is exactly right, and measuring
the whole cross-product of ten receivers against both subscripts turned one
recorded row into eight.

`Interpreter::scalar_subscript_protocol_error` is that enforcement. It is
deliberately **not** the `subscript_descent_refusal` predicate added for section
C2: that one answers "may a chained store *descend* through this slot", where an
`Array` of any kind is a legitimate target. Here the subscript is the last one,
so the question is whether the value does the role at all — and an `Array` does
not do `Associative`. Conflating the two would have regressed every chained
store through an array element.

Both refusal sets are explicit rather than "everything that is not a container",
because a `$` can hold an `Instance` doing `Associative`, a `Buf`, a `Proxy`, a
`Mixin` or a user container subclass, and each has its own store path that must
keep working. The guard is also restricted to `$`/sigilless names: an `@`/`%`
name's sigil already fixes the container kind, and `%h<k> = v` on a real `Hash`
is the whole point.

Two things the measurement changed about the plan:

**A `Range` needed a guard *removed*, not added.** `exec_index_assign_expr_named_op`
refused every subscript store into a `Range` as immutable, positional or not. A
`Range` does do `Positional`, so `$r[0] = 5` really is an immutability error —
but it does not do `Associative`, so `$r<k> = 5` never reaches a store in rakudo
and answers the protocol error instead. That guard is now positional-only.

**The value has to be read through its container.** The predicate consults the
VM local slot first — authoritative for a lexical scalar between env
synchronization points, the same reason the `Range` guard above does — and then
looks *through* the `Scalar`/`ContainerRef` wrapper, or it would classify the
container instead of the value it holds and never fire.

Two rows are left, both recorded as `TODO`s rather than guessed at. A `Buf`
reaches this store as an `Instance` carrying `__mutsu_array_storage` and is
served by the instance arm far above, so it still answers "Index out of range"
where rakudo answers the protocol error; refusing it belongs there. And
`Set`/`Bag`/`Mix` refuse the store with the right class but render it as
"Cannot modify an immutable value (Set)" where rakudo says "Cannot modify an
immutable Set (Set(1 2))" — one of the survey's existing "close but not exact"
rows, untouched here.

Pinned by `t/scalar-subscript-protocol.t`, 28 assertions passing unchanged under
`raku` as well as under mutsu: the refusals, and everything that must keep
working — an undefined scalar and a type object still autovivifying, a `Hash`
and an `Array` still storing, a `Buf` still writing a byte positionally, a user
class doing `Associative` still dispatching `AT-KEY`, and the `@`/`%` names the
rule does not touch.
