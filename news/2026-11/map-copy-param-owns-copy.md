# A `.map` block's `is copy` parameter owns a copy

`@m.map(-> @x is copy { @x.push: 9 })` changed the arrays in `@m`: the map
loops (the inline one and the native writeback lane) bound the block's
parameter to the source element directly, so the signature binder that makes
the copy never ran. A direct call, a `for` loop and a sub with the same
parameter all copied. A block with an `is copy` parameter now takes the full
binding path, so the parameter is the block's own (#18151).

CSS::Specification::Compiler's `Make` builds its link modules with
`@modules.map: -> @module is copy { @module.append: $link-type; ... }` once
per link type; the ids accumulated (`...::Gen::Actions::Grammar`) and its
t/link.t could not load the generated grammar.
