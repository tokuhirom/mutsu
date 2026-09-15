# A `$`-held Slip renders its container in `.raku` — and keeps flattening

`my $x = slip(5, 6); say $x.raku` printed `slip(5, 6)` where rakudo prints
`$(slip(5, 6))`. The `$` container is part of a Slip's `.raku`, the same way it
is for an itemized `Array` (`$[1, 2]`), `Hash` (`${:a(1)}`), `List` (`$(1, 2)`)
and `Seq` (`$((1, 2).Seq)`) — all of which mutsu already rendered. `Slip` was
the one aggregate with no per-value itemization marker at all, so
`Interpreter::itemize_scalar_store` had nowhere to record the store.

Chasing that turned up a second, worse bug underneath it. `.item` and `$( )`
on a Slip fell through to the generic `Value::scalar(...)` wrapper, which is
mutsu's "stop flattening" marker — so `(1, slip(5, 6).item, 2).elems` was **3**
where rakudo says 4, and `my @a = 1, $(slip(5, 6)), 2` produced
`[1, slip(5, 6), 2]` instead of `[1, 5, 6, 2]`. A Slip splices itself into the
surrounding list even out of an item container; wrapping it in a Scalar to mark
the container destroyed exactly the property the type exists for.

## The marker

[#8483](https://github.com/tokuhirom/mutsu/issues/8483) laid out two routes and
priced both: add an `itemized: bool` field to `ValueView::Slip` (329
pattern-match sites across 154 files), or reuse the `Value::scalar(...)`
wrapper `Range` takes (which means "never flattens", i.e. the bug above, and
would need a Slip-shaped carve-out at every site that reads `Scalar` as "stop
flattening"). It asked for a third option: a lighter per-value marker that
other variants could share.

That mechanism already existed, unnamed. `Hash` records its per-holder
itemization as a **second nanbox kind tag** over the same payload —
`Kind::HashItemized` alongside `Kind::HashPlain`, both decoding to the same
`ValueView::Hash` — and `ContainerRef`/`ContainerRefItemized` do it too. The
flag lives in the boxed word, not in the view, so it is invisible to every
consumer that pattern-matches the variant. `Kind::SlipItemized` is the same
trick over the same element `Arc`:

- `ValueView::Slip(items)` is unchanged, so all 329 match sites are untouched
  and a `$`-held Slip keeps flattening everywhere it did before;
- `ValueRepr::Slip` grew the `bool` (7 sites, mirroring `ValueRepr::Hash`);
- `Value::slip_is_itemized()` / `Value::with_slip_itemized(bool)` are the
  accessors, next to the hash pair they copy;
- only `raku_repr` reads the flag.

`itemize_scalar_store_value`, `itemize_value` and `Value::item()` now set it;
`$x<>` and `.Slip` clear it (both hand out the value rather than the
container); `:=` never sets it, since a bind installs no Scalar container. The
empty Slip stays `Empty` with no container, container or not.

`.raku` on a Slip also had **two** renderers — the nested one in `raku_repr`
and a copy in `dispatch_core_repr` for a top-level `.raku` call, each with its
own `Empty` and trailing-comma rules. The copy is gone: the method arm now
delegates to `raku_value`, so the itemization only had to be taught once.

Pinned by `t/collections/itemized-slip-raku-still-flattens.t`, whose 27
assertions pair every rendering check with the flattening check it must not
have cost, and which passes verbatim under rakudo.

Two adjacent gaps this work measured but deliberately left alone, since
neither is Slip-specific: `.self` does not decontainerize an itemized
`Array`/`Hash`/`Slip` ([#8490](https://github.com/tokuhirom/mutsu/issues/8490)),
and the value of a parenthesized assignment expression — `(my $x = [1, 2]).raku`
— is never itemized for any container kind
([#8491](https://github.com/tokuhirom/mutsu/issues/8491)).

Closes [#8483](https://github.com/tokuhirom/mutsu/issues/8483).
