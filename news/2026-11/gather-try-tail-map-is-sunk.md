# The tail of a `try` in a gather body is sunk like a bare `.map`

`gather { try { (1..*).map: { take $_ } } }` died with `X::Cannot::Lazy`: only a
bare trailing `.map`/`.grep` statement got the synthetic sink tail, so the Seq a
`try` body left as its value was forced whole by the end of the gather.
`stmt_yields_lazy_map_grep` now looks through `try` bodies, and the pull
suspends at every `take` as it does for the bare statement. Closes #17099 for
the `try` shape; a sunk map inside a loop op of the gather body still finishes
the loop's iteration per pull.
