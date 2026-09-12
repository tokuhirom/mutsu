# A collection of live element cells kept its elements' own `.gist`

`say @vs.sort` printed `(1.0.0 2.0.0)` where rakudo prints `(v1.0.0 v2.0.0)`, for
`@vs` an array of a `Version` subclass — while `say @vs`, `say @vs.sort.gist`, and
a `for @vs.sort -> $x { say $x }` loop over the very same elements all rendered
correctly. [#8134](https://github.com/tokuhirom/mutsu/issues/8134) suspected
`.sort` of building a Schwartzian transform and then returning the string *keys*
instead of the original elements. It does not: `sort_items_generic`'s no-callable
arm is `items.sort_by(compare_values)`, which never substitutes anything.

## What it really was

`say`/`put`/`print`/`note` choose between pure Rust rendering
(`runtime::gist_value`, which prints an `Instance` as the bare `TypeName()`
placeholder) and real method dispatch (`render_gist_value`, which runs the
class's own `method gist`). The choice is made by probing the collection's
elements — `needs_method_dispatch` and its recursive helper
`element_needs_method_dispatch_seen` in `src/vm/vm_data_io_ops.rs`.

The top-level probe treats a container as transparent and routes it through
method dispatch, with a comment saying exactly why. The **element** probe had no
such arm. It looked through nested Arrays, Hashes and Pairs, but a live
container cell fell off the end of its match into `_ => false`. So every element
producer that hands out cells rather than plain values —
`Value::seq_element_containers` in `src/vm/vm_element_producers.rs`, which backs
`.values`, `.pairs`, `.kv`, `.Seq` and `.sort` — reported "no dispatch needed"
for a collection full of objects, and the whole collection rendered through the
pure path.

The `Version` subclass merely made it *visible*: its `.Str` ("1.0.0") differs
from its `.gist` ("v1.0.0"), so the pure rendering of the element looked like a
plausible-but-wrong value instead of an obvious placeholder. With an ordinary
class the same bug printed the placeholder outright:

```
class F { method gist { "G" }; method Str { "S" } }
my @a = F.new, F.new;
say @a;          # [G G]      -- correct, the Array holds plain values
say @a.values;   # (F() F())  -- rakudo: (G G)
say @a.Seq;      # (F() F())  -- rakudo: (G G)
say @a.sort;     # (F() F())  -- rakudo: (G G)
put @a.Seq;      # F() F()    -- rakudo: S S
my %h = a => F.new;
say %h.values;   # (F())      -- rakudo: (G)
say %h.kv;       # (a F())    -- rakudo: (a G)
```

`.raku` was never affected, which is the detail that identifies the bug rather
than the symptom: its twin probe `contains_dispatch_leaf_seen`
(`runtime::methods_raku_dispatch`) — whose doc comment the gist probe's own
comment points at for "the same discipline" — already looks through both a
`Scalar` and a `ContainerRef`.

## The fix

Three arms on the element probe: look **through** a `Scalar`, a `ContainerRef`
and a `ContainerView` and recurse, rather than answering `true` for every cell
(which would drag a cell of Ints onto the dispatch path for nothing) or `false`
(which is what it did). Pinned by
`t/collections/element-cell-gist-and-str.t`, whose 16 assertions were measured
against rakudo first and cover both halves of the shared probe — the `.gist`
side that `say`/`note` use and the `.Str` side that `put`/`print` use — plus the
`.raku` non-regression, a cell of plain Ints, and the reported `Version`-subclass
repro.

Closes [#8134](https://github.com/tokuhirom/mutsu/issues/8134).
