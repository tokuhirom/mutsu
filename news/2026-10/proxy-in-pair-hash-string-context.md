# A Proxy inside a Pair or Hash FETCHes in string context

A `Pair` or `Hash` value that holds a `Proxy` (an element bound with `@a[0] :=
Proxy.new(...)` and then put into a pair, as `annotations`' `Keeper.hash`
does) rendered as the word `Proxy` through interpolation, infix `~` / `eq`, and
the method-table `.Str` lane. A string context is a read, so it now FETCHes
every nested Proxy first, as the full `.Str` path already did; the lane
declines such a receiver. `annotations` `t/01-containers.rakutest`'s Keeper
subtest compared a Map built that way against a plain Map (#15772).
