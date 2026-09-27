# Array writes, multi-element prepend and splice keep the head offset

#9121 gave `ArrayData` a head offset so `shift` and a one-element `unshift`
are amortized O(1). Everything else still reached the element vector through
`items_mut()`, which hands out the raw `Vec` and so first compacted the dead
prefix away -- an O(e) memmove. A `push` + `shift` queue, or an `@a[$i] = $v`
after a `shift`, paid it on every step. A k-element `unshift`/`prepend`
inserted its elements one by one at increasing indices (each later
`insert(i > 0)` compacted and moved the tail, O(k·e)), `%h<k>.unshift` went
through `Vec::splice(0..0)`, and `splice` drained and then inserted each
replacement element separately (O(e + r·(e − s))). #9156 lists them.

`ArrayData` now has mutators that work on the live range `items[head..]`
directly, so none of them compacts:

- `live_mut()` borrows the live elements for overwrites; every
  `items_mut()[i] = v` and element-rewrite loop uses it.
- `push`/`extend`/`pop`/`insert`/`remove`/`resize`/`truncate`/`split_off`/
  `drain` offset their index by the head instead of compacting.
- `prepend_values(vals)` reserves a front gap of at least k (plus as many
  slots as the array holds, so the next regrow is as far away as the array
  is long) and writes the k elements once: O(k) amortized. Every
  `unshift`/`prepend` path -- the `@` arms, the sigilless `$a` arms, the
  by-value `%h<k>`/`@a[0]` invocant, and the VM's shared-node fast paths --
  calls it.
- `splice_live(start, end, replacement)` is one `Vec::splice` on the live
  range; at the front it advances the head past the removed elements and
  writes the replacement into the dead prefix, so no surviving element
  moves. All three splice implementations call it.

Both new mutators shift the hole bitmap (`initialized`) along with the
elements. The old per-element insert updated it only for the element that
landed at index 0, so a multi-element `prepend` or a `splice` onto an
array with holes left `:exists` answering for the wrong indices.

`items_mut()` remains for whole-vector rewrites. A `NativeBacking`
(ADR-0030) array still shifts with `Vec::remove(0)`; that is the rest of
#9156.

Pinned by `t/collections/array/array-front-mutation-head-offset.t`.
