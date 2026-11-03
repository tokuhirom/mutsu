# `does` in a routine now reaches the caller's Hash/Array variable

`sub lock_hash(\h) { h does R }` (Hash::Util) mixes the role into the caller's `%hash`, as in
rakudo. Two gaps kept the composed value from reaching the caller:

- a plain `@`/`%` parameter the body applies `does` to was never marked as a `does` target, so the
  call kept no source container for it;
- the write-back of a sigilless / `is raw` parameter re-shaped the returned `Mixin(Hash)` into a
  bare Hash (`shape_value_for_sigiled_target`), dropping the role. A `Mixin` over a value that
  already has the target's shape is now kept as is.

Hash::Util's `lock_hash.t` and `lock_hash_recurse.t` now match rakudo (19 and 22 ok);
`lock_keys.t` and `lock_value.t` still differ on other gaps (filed separately). Closes #16231.
