# `pun but Role` / `Class but Role` type objects share their instances' identity

`my \T = Box.^pun but Marker; T.make(1).WHAT === T` was False and `T.WHICH` named the
base (`Box|U..`) while the instances' type said `Box+{Marker}|U..`. A `.WHICH`-keyed type
cache (Monad's) therefore saw two keys for one type. The pun class of a role with
defaulted parameters (`Box\0default[Any]`) is now keyed as `Box`, a `but`-mixed type object
answers its own `.WHICH` (keyed like its composed class), and `===` equates a mixed type
object with its instances' `.WHAT`. Output matches `raku` (#17206).
