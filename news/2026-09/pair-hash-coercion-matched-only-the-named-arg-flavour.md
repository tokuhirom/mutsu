# `(a => 1).Hash` died on "Odd number of elements"

Every coercion of a **bare** `Pair` to a hash — `.Hash`, `.hash`, `.Map` — failed:

```
raku  -e 'say (a => 1).Hash.raku'   # {:a(1)}
mutsu -e 'say (a => 1).Hash.raku'
# Odd number of elements found where hash initializer expected:
# Only saw: a	1
```

A *list* of pairs worked (`((a => 1),).Hash`), which is why the gap survived: the
one-element-list form is what most code writes, and the bare form only shows up
when a `Pair` is passed around as a value.

## Root cause

`to_hash`'s fallback arm (`src/builtins/map_hash_coerce.rs`) matched
`ValueView::Pair` — and since **ADR-0021** that variant is only the *call-site
named-argument* marker. The data flavour a literal mints is `ValueView::ValuePair`,
which fell straight through to `make_odd_number_error`. `items_to_hash`, the
list path, had always handled both, which is exactly why the list form worked
and the bare form did not.

The fallback arm handles both flavours now, keyed the way `items_to_hash` keys a
pair item: a non-`Str` key stringifies, so `(1 => "a").Hash` is `{"1" => "a"}`
and `(<a b> => 1).Hash` is `{"a b" => 1}`, both matching raku.

## A note on the original diagnosis

The ticket this closes guessed the receiver was "arriving as an `Array`/`Slip`
view first" and being flattened by `items_to_hash`, and recorded that a Pair with
a scalar value worked while only a list-valued one failed. **Both halves were
wrong**: `(a => 1).Hash` failed identically, and the receiver never reached
`items_to_hash` at all. The tell was that `(a => 1).Hash` — a shape the ticket
claimed worked — reproduced the error on the first try.

## Verification

`t/pair-hash-coercion.t` pins fourteen assertions dual-oracled against `raku`
v2026.07 (the file's output is byte-identical to raku's): the four coercion
spellings, three key types, ADR-0040's itemization of the stored value (and
`.Map`'s decont of it), the list-of-pairs shapes that already worked, and that a
genuinely odd item list still dies.

Found while verifying ADR-0040 slice 4b, and confirmed pre-existing by rebuilding
`main` — not a regression from that work.
