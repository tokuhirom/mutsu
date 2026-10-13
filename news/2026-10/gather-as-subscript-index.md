# A `gather` used as a subscript index is pulled before it is read

`@a[gather { take 1; take 2 }]` and `<a b c>[gather {...}]` answered `()`: the
postcircumfix index path read the gather `Seq` through pure code, which saw only
its empty seed. The index is now run first, the same way a deferred `.map`/`.grep`
index already was, so both print `(b c)` as in Rakudo. Closes #13069.
