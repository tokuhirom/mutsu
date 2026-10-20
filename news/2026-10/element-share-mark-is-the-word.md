# An element's `=` share is marked by its own word, fixing `.push` and re-binds

An element that `=`-shares a container (`@aoa[0] = @row`) holds the source's
cell. A later `@aoa[0] = 42` has to replace that cell, not write through it.
mutsu recorded which elements were shares in a name-keyed
`__mutsu_elem_share::<name>` env table. That table had two holes:

- `@a.push(@b)` stores the same shared word but never recorded it, so
  `@a[0] = 42` wrote 42 into `@b`.
- A later `@x[0] := $y` left the stale entry in place, so `@x[0] = 6` replaced
  the binding instead of writing through to `$y`.

The element's word already tells the two apart. A share holds the cell
through the itemized `ContainerRef` flavour, while a `:=` bind and the source
variable hold the plain one. That word test (`Value::is_element_share_word`)
now picks replace or write-through at the hash and array stores. The table,
its `mark`/`is`/`clear` helpers, its latch key and `MetaNs::ElemShare` are
gone. This is another ADR-0097 slice (#8087).
