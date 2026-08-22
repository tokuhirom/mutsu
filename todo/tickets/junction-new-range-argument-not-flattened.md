# `Junction.new(TYPE, values)` doesn't flatten a `Range` values argument

Discovered via the doc-diff harness on `raku-doc/doc/Type/Junction.rakudoc` (around line 266).

## Root cause

`methods_object_dispatch_new.rs`'s `"Junction"` arm builds the junction's element list from the
values argument with:

```rust
let elems: Vec<Value> = match values_arg {
    Some(v) => match v.view() {
        ValueView::Array(items, ..) => items.to_vec(),
        ValueView::Seq(items) => items.to_vec(),
        ValueView::Slip(items) => items.to_vec(),
        _ => vec![v.clone()],
    },
    None => vec![],
};
```

Only `Array`/`Seq`/`Slip` are flattened into individual elements; anything else — including a
bare `Range` — falls into the `_ => vec![v.clone()]` arm and is wrapped as a **single** element.
So `Junction.new("one", 1..6)` builds a `one()` junction with one element (the `Range` value
`1..6`, itself truthy), instead of a `one()` junction over the six individual integers `1..6`.

## Minimal repro

```raku
my $n = Junction.new( "one", 1..6 );
say $n.Bool;
```

- `raku`: `False` (a `one()` junction over 6 truthy elements — more than one is true, so `one`
  fails)
- `mutsu` (`target/debug/mutsu`): `True` (treats `1..6` as a single truthy element, so `one`
  trivially succeeds)

Confirmed with `.raku`/gist too:

```raku
$ raku -e 'say Junction.new("one", 1..6).raku'
one(1, 2, 3, 4, 5, 6)
$ target/debug/mutsu -e 'say Junction.new("one", 1..6).raku'
one(1 2 3 4 5 6)
```

(mutsu's gist format for the range case is also slightly off — no commas/space — but the real
bug is the element count/identity, confirmed via `.Bool` above.)

## Affected files (starting point)

- `src/runtime/methods_object_dispatch_new.rs` — the `"Junction"` arm's `elems` match (around
  line 1275-1283 as of this writing). Needs a `ValueView::Range(..)` arm (and likely any other
  general iterable) that flattens like `Array`/`Seq`/`Slip` do, rather than falling through to
  the single-element wrap.
