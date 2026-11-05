# A `my %*ENV` declared in a block is read back by element subscripts

`{ my %*ENV = A => 'x'; %*ENV<C> = 'c'; say %*ENV<C> }` read `Nil` for the declared hash's elements
(#16681). The `%*ENV` element-store fix of #16559 reads the declared hash too, so the block form
matches `raku` on `main`; this adds the regression test that pins it. Closes #16681.
