# `Seq.ACCEPTS` consumes its receiver

`$seq.ACCEPTS(@list)` now consumes the Seq (and a Seq topic) the way Rakudo's
does, so a second call throws `X::Seq::Consumed`. A topic that is not a list is
still an identity test and leaves the Seq readable (#13332).
