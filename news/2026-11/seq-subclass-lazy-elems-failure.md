# `.elems` / `.end` of a lazy `Seq` subclass fail instead of hanging

A user class of `Seq` whose `is-lazy` answers True (ParaSeq over `^Inf`) had
`.elems` and `.end` drain the endless iterator, so the process never returned.
They now answer a `Failure` wrapping `X::Cannot::Lazy` (action `.elems` /
`.end`), as rakudo's `Sequence.elems` does. ParaSeq's `t/03-elems.rakutest` ran
into its timeout before and now takes 4 seconds.
