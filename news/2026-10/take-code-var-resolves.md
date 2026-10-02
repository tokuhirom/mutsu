# `&take` now resolves to a callable

`&take` used to evaluate to `Nil`, so `@a».&take` died with `No such method ''`,
and `.map(&take)` / `my &f = &take` did not work either. `take` now resolves to
a routine like `return` does, applying the existing `take` builtin, so
`gather { (1,2,3)».&take }` yields `(1 2 3)` and a call outside a gather reports
`take without gather` (#10889).
