# Attributive parameters respect sigil-colliding attributes

A class may declare `has %!c; has $!c;` -- two distinct attributes that share a
bare name. The instance store keeps such a pair under sigil-qualified keys, but
the attributive-parameter binders (`submethod BUILD(:$!c)`, `method m($!c)`)
wrote the value under the bare name regardless, overwriting the other
attribute's slot. `Mux` (zef) does exactly this with `%!channels` / `$!channels`
and so ran with its worker-channel table replaced by the `:channels` Int, which
made its `t/01-full.t` dispatch order flaky. Both binders (the method fast path
and the generic env-level binder) now resolve the storage key through
`attr_key_in_map`, the same resolution ordinary `$!c` reads and writes use.
Pinned by `t/oo/attributive-build-param-sigil-collision.t`.
