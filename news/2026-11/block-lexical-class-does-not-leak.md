# A `my class` in a `do` block or a `.map` callback stays in that block

```raku
class Inner { method tag { "outer" } }
(1..2).map({ my class Inner { method tag { "map" } }; 1 }).eager;
say Inner.tag;   # was map, now outer as in Rakudo
```

A value-position block (`do { ... }`, and the block a `.map`/`.grep`/`.first`
callback is recompiled into for the inline loop) reverts the declarations it
owns when it exits, but a `my class` / `my role` was not among them, so its
name stayed bound to the block's type for the rest of the enclosing scope.
The block now records a lexical type's bare name and its type-only key as its
own declarations, and a callback that declares one is compiled as such a block
(#18178).
