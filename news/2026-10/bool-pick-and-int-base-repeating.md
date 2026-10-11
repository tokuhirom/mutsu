# Defined Bool.pick/roll and Int/Num.base-repeating now fail like Rakudo

`True.pick`, `False.roll` and friends die with the `Bool:U`-only candidate list, and
`base-repeating` is answered by `Rat` alone (`Int` and `Num` raise `X::Method::NotFound`).
