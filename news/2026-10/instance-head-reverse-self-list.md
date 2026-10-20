# `.head` and `.reverse` of a user instance read its list

`Any.head` and `Any.reverse` are `self.list.head` and `self.list.reverse`, but
on an instance of a user class (whether it `does Positional`, `Associative` or
nothing) mutsu answered `X::Method::NotFound`: the pure `Any` rows decline an
instance, and no interpreter twin took the call. The `Any` twins now read the
instance's `list` (its own, or `Any`'s one-element list of itself) and ask it,
so `$el.head` and `$el.reverse` answer as in Rakudo
(#13819). XML::Class's serialisation suites, which call `.list` and `.head`
on `XML::Element does Positional` values, now pass.
