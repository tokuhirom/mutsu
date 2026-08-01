# A hyper on an itemized Hash maps over its values

```raku
my %h = a => 1, b => 2;
say (%h>>.Str).raku;        # {:a("1"), :b("2")}   -- always correct

my $g = ${a => 1, b => 2};
say ($g>>.Str).raku;        # raku:  {:a("1"), :b("2")}
                            # was:   ("a\t1\nb\t2",)
my $i = %h.item;
say ($i>>.Str).raku;        # same, so it was the itemization, not the literal
```

mutsu stringified the whole hash and wrapped it in a one-element list: it never
reached the hyper's Hash branch and fell through to the generic element path,
where an itemized value is one element.

## Why the gate missed

The ticket's first step was to find out what the view actually is at the
`hash_keys` gate, because `$g.WHAT.^name` answers `Hash` and
`runtime::utils::value_to_list` matches
`ValueView::Hash(_) if val.hash_is_itemized()` — so itemization looked like a
flag *beside* a `Hash` view, which the gate would have matched. A breakpoint on
the gate settled it in one run: `hash_keys` really was `None`, and `items` really
did hold one element.

Both are true at once because mutsu itemizes in two different ways.
`Value::item` sets a flag on the value for an `Array` (`ArrayKind::ItemArray`)
and for a `Hash` (`ValueRepr::Hash(h, true)`) — and those the view does see
through — but wraps **everything else** in a `Scalar`. The hash reaching the
hyper is a `Scalar`-wrapped one, from `${...}` and from `.item` alike, and no
container gate in the hyper unwraps it. The Hash-keys gate missed, the QuantHash
arms missed, and the target went to `hyper_source_items` → `value_to_list`, which
answers a different question (how many elements a value contributes to a
flattening list assignment) and correctly calls an itemized value one element.

## The fix

Itemization is a property of the container a value *sits in*, not of the thing
`>>` is walking, so Rakudo hypers straight through it. Both entry points —
`exec_hyper_method_call_op` and `exec_hyper_method_call_dynamic_op` — now strip
it from the target once, in the shared `Interpreter::hyper_target`, before any
container gate runs. That is deliberately *not* a change to
`hyper_source_items`: a Hash has to keep its keys so the hyper can rebuild a Hash
from the per-value results, which is exactly why the ticket ruled that route out.

`hyper_target` also reports whether the target *was* itemized, because the
mutating postfix hypers need it. `$q>>++` on a scalar-held hash wrote its result
back through `overwrite_hash_bindings_by_identity`, an identity scan that only
sees plain-`Hash` bindings — so it missed the `Scalar`-wrapped one and the
increment was silently lost. It now writes back by name, re-itemized, so `$q`
stays a `${...}` holding the incremented values.

The itemized-**list** twin ([news](../2026-07/hyper-descends-into-an-itemized-list.md))
needed nothing: an itemized `Array` carries a kind flag rather than a wrapper, and
`hyper_source_items` already asks for its own elements. Per-element itemization
is untouched too — `itemize_if_descended` restores it on each result.

`t/hyper-itemized-hash.t` pins 14 assertions: the three spellings of an itemized
hash, that the result is a Hash with the original keys rather than a one-element
list, a hyper with arguments, `>>++`/`>>--` write-back through the itemization,
an itemized `Set`, and the two behaviours that must *not* change (an itemized
list still hypers into itself; list assignment still counts it as one element).
Every one also passes unmodified under rakudo.

## Found on the way

A hyper over a `Bag` or `Mix` drops the weights — `<a a b>.Bag>>.Str` is
`("a"=>0,"b"=>0)` where raku keeps `a => 2, b => 1`, and a `Mix` whose weights
all collapse to 0 renders as the empty `mix()`. That is not itemization (the
plain form is equally wrong) but the QuantHash result-rebuilding tail, so it is
fixed separately in
[hyper-over-a-bag-or-mix-maps-the-weights](hyper-over-a-bag-or-mix-maps-the-weights.md).
