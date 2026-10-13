# A subscript on a deferred `.map`/`.grep` Seq runs the callback only as far as the index

`$s[5]` on `(1..100).map({ ...; $_ * 2 })` ran the callback over the whole source.
`reify_seq_body_prefix` now pulls a bounded prefix of a `MapGrep` body through
`extend_map_grep_prefix`, so the callback runs once per element read, as in Rakudo
(#13166).
