# Dispatch role-mixed collection elements while rendering

Fixed Array, List, Seq, Hash, Pair, and related collection rendering so a role-mixed element is no longer sent through the primitive-only gist fast path. Collection rendering now recognizes Mixin elements as dispatch-capable and invokes their role-provided `gist` or `Str` behavior while preserving the surrounding collection's normal bracket style.

This restores examples such as `say [5 but role :: { method gist { "tagged" } }]` without changing storage, flattening, or the Mixin's underlying native value.
