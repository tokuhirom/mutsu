# `.Slip` on an unforced `gather` Seq

`(gather { take 1; take 2 }).Slip` answered `Empty`: the pure coercion read the
LazyList's still-empty cache. The VM now forces a coroutine-backed gather before
slipping it. Found via the `Directory` distribution, whose `list` method does
`@entries.push: $!iodir.dir.Slip`; its `t/01-basic.rakutest` now passes.
