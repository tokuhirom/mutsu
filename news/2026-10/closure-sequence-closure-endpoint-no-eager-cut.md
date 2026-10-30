# A closure sequence with a closure endpoint is no longer cut at its first big element

The eager-prefix size cut of #15324 is only sound where the sequence hands
back a lazy continuation (no endpoint, or a value endpoint). With a closure
endpoint (`seed, { ... } … { cond }`) the result is a plain array, so the
cut truncated it at the first element above 4096 bits, and Math::Root's
`newton` returned its seed. The cut now applies only to the lazy cases (#15424).
