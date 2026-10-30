# `CLIENT::<$*X>` reads the calling package's scope

`CLIENT::<$*INDEX>` returned `Nil` because only `CALLER::` was resolved to a caller
frame. `CLIENT::` is now the same lexical read, taken from the nearest caller frame
that lives in a different package than the running routine (the frame depth is found
when the op runs). `Prompt::Expand`'s `:$index = CLIENT::<$*INDEX> // 0` default, which
expands `:index:` and `:symbol:`, now matches rakudo: all 10 assertions of its
`t/01-basic.rakutest` pass.
