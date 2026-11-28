# `%h{$k} = $v` into a declared local takes the lean store lane

The lean element store (#19306) served only `@a[$i] = $v`. A store into a declared `%` local still
went through the general preamble and the env-rooted hash lane: ~3,700 instructions per store with
an `Int` key, plus a heap `String` built from the integer for the key on every store.

The lane now serves a plain user `%` lexical too, with a `Str` or `Int` key. It checks the hash's
own metadata (no type meta or typed keys, no default, not a bare-values or storage-cells hash) and
that the entry is absent or a plain value, then writes in place, as the general store does for a
shared node. `Value::hash_insert_through_str` takes the key by reference, so overwriting a present
entry builds no key `String`; it shares its entry write with `hash_insert_through`.

Measured with callgrind on 100,000-iteration loops (#19285, #19289): an `Int`-key store costs
~2,700 fewer instructions (marginal ~3,700 → ~1,000) with 0 allocations instead of 1, and a
`Str`-key store ~3,300 fewer. `bench-hash` runs 16% fewer instructions, and a tenth-size
`bench-index-store` 13% fewer.
