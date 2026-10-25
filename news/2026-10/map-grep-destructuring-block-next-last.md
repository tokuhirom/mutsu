# `next`/`last` in a destructuring map/grep block, and a Pair in an item container

`Red::Column.raku` builds its text with
`.map(-> (:$key, :$value) { next if ...; ... })`. The full-binding call path of `map`
(List and Array receivers) and of `grep` returned the loop-control signal out of the loop,
so a `next`/`last` in a block whose signature destructures its argument surfaced as an
uncatchable `X::ControlFlow`; they now act on the element, as they do in every other block.

The same Red run exposed a second bug: after a `grep` visited an Array, the Array's elements
sat in item containers, and a later map/grep block with a destructuring signature looked up
`.key`/`.value` on the container and bound `$key`/`$value` undefined (every Red `ResultSeq`
row). Destructuring now takes a container's content apart, not the container.
