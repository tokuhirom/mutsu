# A negative seek fails with MoarVM's message

`IO::Handle.seek` to a negative position raised a made-up `X::IO::Seek: Cannot seek to a negative
position`. Rakudo throws an `X::AdHoc` with `Failed to seek in filehandle: 22` (EINVAL); other
`lseek` failures carry their errno the same way. Found by the raku-doc differential
(`Type/IO/CatHandle.rakudoc:548`).
