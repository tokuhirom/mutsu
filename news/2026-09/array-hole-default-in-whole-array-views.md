# A hole in an `is default(...)` array renders as the default in `.raku`, `.gist`, `.Str` and `.join`

```raku
my @m is default(7) = 1, 2, 3;
@m[0]:delete;
say @m.raku;   # [7, 2, 3]   (was [Any, 2, 3])
say @m[0];     # 7
```

`@a[$i]` already read a hole (a `:delete`d slot, or a gap an out-of-range store
grew) as the container's `is default(...)` value, but the whole-array views read
the raw slot, which holds the `Any` hole marker. Issue #10317 reported it for
`.raku`; the same marker leaked through every renderer.

The slot keeps holding the marker on purpose. `ArrayData::hole_at` tells a hole
from an explicit element by that marker plus the `initialized` set, and not
every writer maintains `initialized` (`push`/`append` do not), so a hole test
based on "the slot holds the default value" would report an explicitly pushed
`7` as absent. The substitution therefore happens on the way out, through one
helper, `ArrayData::items_with_default`, which borrows the elements untouched
for an array with no `is default` value or no hole and copies with the holes
replaced otherwise.

It is used by the chokepoints that render or list an array: `value_to_list`
(both copies), `.raku`, `.gist` and `say` (the pure renderer, the native
`.gist` arm and the method-dispatch renderer that an array holding a
`Package` marker is routed to), `.Str` / `~@a` / `put`, `.join`, and the typed
`Array[T].new(...)` form of `.raku`. `.List` still keeps a hole as `Nil` and
`:exists` still reports it absent, both as Rakudo does, and
`t/collections/array/array-delete-hole-reads-is-default.t` pins that too.

What is left is filed as #10360: the many other raw element reads (`.values`,
`.kv`, `.map`, `for`, slices, `my @c = @m`, ...) need one representation
decision rather than more call-site patches, and `for` / `.Seq` / `.values`
over such an array also drop the default from the array node, which a
view-only fix cannot survive.
