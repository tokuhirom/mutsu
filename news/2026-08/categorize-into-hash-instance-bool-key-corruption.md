# An object hash gists its typed keys even when a value has a custom `.gist`

Discovered via the doc-diff harness on `raku-doc/doc/Type/Any.rakudoc:961`.
`my %h; class Foo {}; my @a = (Foo.new,); @a.categorize({True}, into => %h); say %h`
printed `{Bool|1 => [Foo.new]}` where `raku` prints `{True => [Foo.new]}`. The
same script with `@a = (1,)` printed correctly, which is what made the ticket
suspect classify/GC interaction.

## Root cause

Nothing to do with `categorize`, and nothing to do with the source array holding
instances *per se*. The bug was in **hash gist rendering**, and it reproduces
without classify at all:

```raku
class Foo {}
my %h{Any};
%h{True} = Foo.new;   # mutsu: {Bool|1 => Foo.new}     raku: {True => Foo.new}
%h{True} = 1;         # mutsu: {True => 1}             -- correct
```

mutsu has two hash-gist renderers. The pure one
(`builtins/methods_0arg/dispatch_core_repr.rs`) correctly resolves an object
hash's stored `.WHICH` string key back to the original typed key via
`map.typed_key(k)`. The *dispatching* one
(`runtime/methods_call_dispatch.rs`'s `gist_item`) — which takes over as soon as
any value anywhere in the subtree is an instance that may define its own
`.gist` — formatted the raw stored key instead.

So the discriminator was the **value**, not the key: putting an instance
anywhere in the hash switched rendering to the path that leaked `Bool|1`. The
earlier investigation's observation that `dd`/`.raku` looked correct fits — those
go through the `.raku` renderer, which already did the lookup.

## Fix

`gist_item`'s `ValueView::Hash` arm now renders each key as
`gist_item(interp, &map.typed_key(k))`. `typed_key` returns a plain `Str` for a
non-object hash, and `gist_item` short-circuits to the pure renderer for any
subtree with no instance in it, so ordinary hashes are unaffected — and an
object hash keyed by an *instance* with a custom `.gist` now renders that key
through its own method too, which the pure path could not do.

`t/buf-and-list-mutators.t` pins the direct object-hash form for a `Bool` and an
`Int` key, plus the original `categorize(… , into => %h)` repro.
