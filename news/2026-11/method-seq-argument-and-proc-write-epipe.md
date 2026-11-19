# A method's `.map` argument stays lazy, and `Proc::Async.write` reports a closed pipe

A `.map` Seq passed to a method a user class declares was run to its end at
the call boundary, before the method had read anything; a sub call already
kept it lazy. The method's own signature binder now pulls it as far as the
parameter needs (a slurpy still flattens it), while native methods such as
`@a.splice(1, 1, SEQ)` keep reading the elements up front.

`Proc::Async.write` (and `.print`/`.say`/`.put`) kept its Promise even when
the bytes never reached the child. It is now broken, as in Rakudo, with
`broken pipe` once the child closed its stdin and `Cannot write to process
that has already terminated` once it was reaped.

Together these let Rakuish's `Stream.exec` feed a 200 000-row generator to
a child that stops reading early: it used to generate the whole stream
first and hang, and now stops after a few thousand rows and flags
`in-early` (Rakuish t/48-exec-stdin.t).
