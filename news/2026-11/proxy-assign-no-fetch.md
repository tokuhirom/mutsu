# Assigning to a Proxy runs only its STORE

`$p = 2` on a Proxy ran its `FETCH` after the `STORE`, to produce the
assignment's value, even when nothing read that value. Through a sub's or a
method's `is raw` parameter the store fetched once more. A `FETCH` with side
effects (AttrX::Mooish's lazy `AttrProxy` builds its attribute there) ran
more often than on rakudo.

The assignment sites now run the `STORE` alone. An expression form leaves
the Proxy container as its value, which `FETCH`es only when it is read:
`my $r = ($p = 4)` fetches once, as in rakudo (#18337).
