# `.encode($encoding)` is a method-table row; the VM's encode/decode entry is gone

`Str.encode` and `Cool.encode` with an encoding name, a normalization form or
`:replacement` are now interpreter rows of the method table (ADR-11276), next
to the existing zero-argument `encode` row and the `Blob`/`Buf` `decode` rows.
That made the VM's own `try_native_encode_decode` pre-check — a name-matching
entry consulted on both the plain and the variable-receiver method-call paths
before the generic fallback — redundant, so it and its two call sites are
deleted, together with the cascade's `dispatch_decode` wrapper it was the only
caller of. Encoding and decoding now have one route each: the row.
