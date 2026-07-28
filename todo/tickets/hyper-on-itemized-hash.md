# A hyper on an itemized Hash does not map over its values

```raku
my %h = a => 1, b => 2;
say (%h>>.Str).raku;        # {:a("1"), :b("2")}   -- correct

my $g = ${a => 1, b => 2};
say ($g>>.Str).raku;        # raku:  {:a("1"), :b("2")}
                            # mutsu: ("a\t1\nb\t2",)
my $i = %h.item;
say ($i>>.Str).raku;        # same, so it is the itemization, not the literal
```

mutsu stringifies the whole hash and wraps it in a one-element list — i.e. it
never reaches the hyper's Hash branch and falls through to the generic element
path, where an itemized value is one element.

## Where it is

`exec_hyper_method_call_op` (`src/vm/vm_hyper_method_ops.rs`) gates the Hash
handling on

```rust
let hash_keys: Option<Vec<String>> = if let ValueView::Hash(map) = target.view() { … }
```

and that gate does not match for an itemized hash, so `hash_keys` is `None` and
the target falls through to `hyper_source_items` → `value_to_list`, which
(correctly, for its own purpose) treats an itemized hash as one element.

The first step is to find out **what the view actually is** there. `$g.WHAT.^name`
answers `Hash` and `runtime::utils::value_to_list` matches
`ValueView::Hash(_) if val.hash_is_itemized()`, so the itemization is supposed to
be a flag beside a `Hash` view rather than a different variant — yet the gate
misses. Either the stack value is wrapped (a `Scalar`/`ContainerRef` the hyper
does not unwrap) or the itemized hash is built as something else entirely; a
`--dump-ast` plus a breakpoint on the gate settles it in minutes.

## The fix is *not* `hyper_source_items`

The itemized-**list** twin was fixed by asking for the node's own elements
([news](../../news/2026-07/hyper-descends-into-an-itemized-list.md)), but a Hash
cannot reuse that: the hyper must keep the keys so it can rebuild a Hash from the
per-value results (`%h>>.uc` is a Hash, not a list of pairs). So the fix belongs
at the `hash_keys` computation, which needs to recognise the itemized form.

Both hyper entry points have their own copy of this gate — `exec_hyper_method_call_op`
and `exec_hyper_method_call_dynamic_op` — and they must stay in step.

## Worth checking while there

Whether `%h>>!`/`%h>>++` (the postfix hypers, which share the Hash branch) and
`>>.method(args)` behave the same way on an itemized hash, and whether an
itemized `Set`/`Bag`/`Mix` has the same gap.
