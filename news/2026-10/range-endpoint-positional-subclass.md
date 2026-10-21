# A Range endpoint that subclasses Array or Hash numifies to its element count

`^$a`, `0..^$a` and `0..$a` kept an `Array`/`Hash` subclass instance (and a role
mixin of one) as the Range endpoint itself instead of numifying it, as a plain
Array endpoint is. `^max(@!index, self)` in PDF's `Tie::Array.check` iterated
over a nonsense range, so `.check` never visited the entries and never died on
an invalid one (`t/cos-tie-index.t`).
