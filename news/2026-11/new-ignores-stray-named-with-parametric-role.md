# `.new` ignores a stray named argument when the class composes a parametric role

A class composing a parametric role (`class B does Q[2]`) carries the role's
instance name (`Q[2]`) in its MRO, which the "attribute set is fully known" check
did not recognise, so `B.new(:0reserved)` stored `reserved` as a slot and
`B.new(:0reserved) eqv B.new` was False. The check now resolves the instance name
to its registered base role, so the default constructor drops the name as rakudo's
`BUILDALL` does (#16903).
