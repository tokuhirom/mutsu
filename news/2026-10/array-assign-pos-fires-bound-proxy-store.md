# `Array.ASSIGN-POS` stores through a Proxy bound into the slot

`@a.ASSIGN-POS($i, $v)` replaced a slot that had been `BIND-POS`-bound to a
`Proxy` instead of firing the Proxy's `STORE`, unlike `@a[$i] = $v`. It now
mediates the store through the Proxy and answers the fetched value. The
`annotations` distribution's `Buffer.ASSIGN-POS` forwards to its backing array
this way, so assigning to a slot whose `Binder` holds a read-only value now
leaves it untouched, as on rakudo.
