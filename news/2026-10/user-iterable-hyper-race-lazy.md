# `hyper`, `race` and `lazy` of a user `Iterable` read its `iterator`

`class It does Iterable { method iterator { (1,2,3).iterator } }; It.new.hyper` was
`X::Method::NotFound` since the `race`/`hyper` cascade arms were retired. Those `Iterable`
methods now pull the instance's own `iterator` and answer from the resulting list. `.List` of
such an instance stays a one-element list, as in Rakudo (the issue text had that wrong) (#12929).
