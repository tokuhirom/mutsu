# An aggregate assigned into an element reads back itemized, and a List element's hash flattens

An `Array` or `Hash` element is a `Scalar` container in Raku, so an aggregate
stored in one is itemized: `my @a; @a[0] = %h; @a[0].raku` is `${:a(1)}`, and
`my %c = (@a[0],)` dies "Odd number of elements" because the element is one
opaque item. mutsu shares the source's container into the element for
`@a[0] = %h`, and that shared word carried no itemization, so the element read
back as the plain `{:a(1)}`. A hash initializer only got the dying case right
because it refused to look inside an element container at all. That same
refusal made a `List` element's hash (`my @l := 1, %h; my %c = (@l[1],)`) die,
where rakudo flattens it.

The element's own copy of the shared word is now tagged as an itemized holder
(ADR-0079 slice 3, for the element share). Binding, subscript reads and
`.raku` keep that tag instead of rebuilding a plain word. The hash initializer
now unwraps every element container unconditionally (ADR-0079 slice 4).
`@a[0].raku`, `%k.raku`, `(@a[0],)` and `(@l[1],)` all match rakudo, and the
source `%h` itself still renders `{:a(1)}`.
