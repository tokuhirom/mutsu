# A sigilless callee's write reaches a typed routine's local

A sigilless parameter (`\v`) aliases a variable handed to it, so `sub set(\v) { v = 5 }`
called as `set($a)` changes `$a`. When the caller was a typed routine running in TRIR, the
generic call site passed only `$a`'s value. The callee's write then died with "Cannot modify
an immutable Int" on every call after the first; the first runs untyped, before its call
site reaches TRIR.

A TRIR generic call already hands over a container, and reads it back afterwards, for an
argument whose parameter is `is rw`. A sigilless parameter now counts as one too
(`rw_positional_mask`), so the callee's write lands in the caller's slot as it does on the
untyped path. A value argument stays a value and is still read-only on the callee's side.
Found while working on JSON::Fast's encoder (#8673).
