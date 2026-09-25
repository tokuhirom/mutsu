# List methods stop copying the whole invocant to answer a small question

Closes [#9162](https://github.com/tokuhirom/mutsu/issues/9162).

A group of list methods needed O(1), O(k) or O(i) of their invocant, but each
one first copied or decomposed all of it (`value_to_list`,
`value_to_list_for_receiver`, `array_element_cells`, or a full Proxy /
instance pre-scan). One call was O(e), so a loop that called the method on a
growing array was O(n²). The standard case is a stack that is pushed and then
read with `@stack.tail`.

The shared fix is to borrow the items the value already holds instead of
rebuilding them. `runtime::utils::with_list_items` /
`with_receiver_items` hand a caller the `&[Value]` of an Array/List or a
reified Seq, and fall back to the old copy only for shapes that do not store
their items reified. `list_items_len` reads the length the same way. Each
site:

- **`.first`** scans in chunks that double in size (from the end for
  `:end`). A hit at index i decomposes O(i) elements and pays O(log i)
  matcher setups. A mutable array still hands the matcher its element
  containers, so `@a.first({ $_ = 5 })` writes `@a`. The length is re-read
  for each chunk, so a matcher that shrinks the array is safe.
- **`.pick`, `.roll`, `.roll(k)`** index the borrowed slice.
- **`.head(k)`, `.tail`, `.tail(k)`** slice the borrowed items. That now
  covers any list-like, including `@a.tail` on a named array, which reaches
  `dispatch_tail`.
- **`.skip(...)`** resolves its skip/produce specs against the length alone.
  It turns them into index spans and clones only the produced items.
- **`eqv`** on two Array/List operands compares kind and length first, then
  walks the element pairs in lockstep, resolving any `Proxy` pair by pair.
  A length mismatch is O(1) and an early difference O(i). Before, both
  operands were walked in full by the Proxy pre-scan.
- **`@a == @b`** compares the lengths in place.
- **`.gist`, `say`, `note`** probe only the rendered head. The dispatch
  probes (`collection_contains_instance`, `gist_route`, `contains_cycle`) now
  stop at `GIST_ELEM_CAP` elements of each list. `say`/`note` cut a long
  list down to that head (`gist_head`, which keeps the element type, default
  and holes) before the Proxy FETCH and the Rat/Failure checks. Rakudo never
  renders, FETCHes or checks an element past the head either.
- **`.List` on a List** hands back the same immutable storage (Rakudo's
  `List.List` is `self`). **`.cache`** on a List returns the List, and on a
  reified Seq it returns a List view over the Seq's own body instead of a
  copy.
- **`.squish(:as/:with)`** used to detach (copy) every Array/Hash lexical in
  scope to snapshot the env for its revert. It now detaches only the
  callbacks' free variables. Other bindings are compared by identity, and
  the env is diffed in full only when a callback actually rebinds a lexical.

Measured with `scripts/array-complexity-check.sh` on a release build. Every
fixed-call-count case in the issue's table now has a t(2N)/t(N) ratio of
about 1, like the `.head(3)` control. The `-- see #9162` annotations are
gone from `src/`.

Pinned by `t/collections/list-methods-read-only-what-they-need.t`.
