# The accessor lane replays Array and Hash attribute reads

`$obj.children` on a `%.children` attribute missed the per-layout accessor lane
(it replayed scalar attributes only) and ran the whole `try_fast_accessor_read`
gate chain on every read: ~4k Ir per call, 35 such reads per inserted word in
`ML::TriesWithFrequencies` `trie-create`.

A lane entry now also carries what the full path decides for an aggregate,
settled once per declaration: whether the accessor is `is rw` (the container is
returned as stored; otherwise it is decontainerized) and whether the attribute
declares an element type (then the stored container must already carry it, else
the read takes the full path, which tags it). See #15235.
