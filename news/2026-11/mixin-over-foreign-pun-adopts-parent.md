# A mixin over a pun made on another thread composes the pun as its parent

`Box.^pun but Marker` built on one thread and constructed from on another failed with
`'Box+{Marker}' cannot inherit from 'Box default[Any]' because it is unknown`, because the
class-parent check never tried the published pun recipes (ADR-11456). It now adopts the parent
from its recipe before judging the name unknown. Part of #17149 (Monad's cached
`pun but Some` types).
