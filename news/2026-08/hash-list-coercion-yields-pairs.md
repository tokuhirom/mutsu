# `%h.List` yields the Hash's pairs

`.List` on a Hash returned a one-element list holding the whole Hash
(`({:a(1)},)`) instead of the Hash's pairs (`(:a(1),)`). The `"List"` arm of the
native coercion table had no `Hash` case, so a Hash fell through to the scalar
catch-all that wraps its target as a single element — even though the sibling
`.list` and `.Array` arms had had a `Hash` case all along, so
`%h.List` and `%h.list` disagreed.

`Set`, `Bag` and `Mix` were wrapped by the same catch-all and are fixed with it:
`.List` on those now yields their `key => weight` pairs, matching `.list`.

This is one of the two independent reasons `Cro::HTTP::Client` rejected every
request that passed `headers => %h`. The client does

```raku
when 'headers' {
    self!set-headers($request, $value.List) if $value ~~ Iterable;
}
```

and `set-headers` then requires each element to be a `Pair` or a
`Cro::HTTP::Header`, so it saw a Hash and threw
`X::Cro::HTTP::Client::IncorrectHeaderType`. (The other reason — a `Pair` read
out of a variable being passed as a *named* argument — is recorded in
`todo/deep/pair-namedness-is-a-value-property-not-a-call-site-property.md`.)

Pinned by `t/hash-list-coercion.t`.
