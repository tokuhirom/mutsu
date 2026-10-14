# A routine's own object hash no longer re-types its caller's hash

`sub f { my %seen{Str}; ... }` called from a scope with its own
`my %seen{Int}` left the caller's hash accepting `Str` keys after `f`
returned. The key type lives in an env metadata lane
(`__mutsu_hash_key_type::%seen`), and unlike the value-type lane it was not
marked as shadow metadata, so the return merge copied the routine's lane back
into the caller as if it were a write to a shared variable.

The hash-key lane is now shadow metadata like `__mutsu_type::`: the return
merge and the closure capture decide it by the hash it describes, so a
routine's own declaration stays in its frame, and a closure still checks
keys against the hash it closes over (#13139).
