# A `sub ($self)` nested in a `for -> $self` body is pinned

#15464 moved the loop `$self` parameter onto the reserved `$self` lexical, which
removed the `self_is_signature_param` mapping #15482 describes; the nested
`sub ($self) { ... }` case now matches rakudo. This adds the missing regression
case to `t/vm/scope/lexical-self-vs-invocant.t`.
