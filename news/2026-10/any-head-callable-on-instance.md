# `Any.head(*-N)` on an instance reads its own list

`Any.head(&callable)` is `self.list.head(&callable)`, but only `tail` took that
route for an instance: `E.new.head(*-1)` died "No such method 'head'", and an
instance of a class with `multi method head(Int $n)` died "none of these
signatures matches" instead of reaching the inherited `head`. Red's
`ResultSeq` (`multi method head(UInt(Numeric) $num)`) hit it with `$rs.head(*-5)`
in `t/38-equal`. The `head` twin now counts an instance's end-relative or
`Whatever` argument against the instance's own list, as `tail` does.
