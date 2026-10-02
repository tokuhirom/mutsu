# A constant aliasing a package now qualifies a sub call

`my constant Meta = A::Meta; Meta::index()` and `&Meta::index` failed with
`Could not find symbol '&index' in 'GLOBAL::Meta'` (or silently yielded an
undefined `&` value), because the leading component of a qualified routine name
was only looked up as a package name. The qualified-call fallback and the
`&Alias::sub` term now resolve a leading constant bound to a package type object
and retry under the real package, as raku does. This unblocks CSS::Module's
`:index(&Metadata::index)` shape (#10911).
