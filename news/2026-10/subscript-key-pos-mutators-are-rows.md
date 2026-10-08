# The subscript protocol's mutators are rows

`ASSIGN-KEY` and `DELETE-KEY` on `Hash` and the six quant hashes, and
`ASSIGN-POS` and `DELETE-POS` on `Array`, are `Handler::Mut` rows of the method
table now (ADR-11276 slice 4). The VM's by-name arms, the `is Hash` /
`is Array` / `is BagHash` `nextsame` bridges, the `Mixin` bridge and the
by-value cascade each carried their own copy; one handler per owner answers a
named binding, a detached container and the backing storage behind a user
subclass alike. About 450 lines of duplicated mutation code are gone.

One behaviour moves toward Rakudo: `push`/`append` reached by `nextsame` from a
user `is Hash` subclass stack a repeated key instead of overwriting it.
