# Tied sub-signature multis resolve by declaration order

`multi sub k(@ ($x where * > 5)) {...}; multi sub k(@ ($x)) {...}` died with
`Ambiguous call` for `k([9])`. Both candidates have the same nominal shape. In
rakudo, a destructuring sub-signature makes a candidate a bind-check candidate,
like a `where` clause, a subset or a literal does. Rakudo trial-binds tied
bind-check candidates in declaration order and the first that binds wins. So
the `where` nested in the first candidate decides between `big` and `small`.

mutsu now treats a destructuring sub-signature as a bind-check candidate for
multi subs and multi methods (#11027). Multi methods also stop reporting tied
`where`-constrained candidates (`multi method m($x where True)` twice) as
ambiguous, which they used to do unlike multi subs. Plain positional duplicates
(`multi f($x)` / `multi f($y)`) remain `X::Multi::Ambiguous`, as in rakudo.
