# `append`/`push`/`unshift`/`prepend` on a Buf reached through an accessor, hash or array element

`$obj.buffer.append(...)` (a `has buf8 $.buffer`), `%h<k>.append(...)` and
`@a[0].append(...)` mutated a copy and left the stored `Buf` unchanged, because
a receiver with no variable binding took the "mutate a temporary" path. Such a
receiver is the object its container holds and rakudo grows that object; the
path now extends the shared buffer storage in place (a `Blob` stays immutable).
Net::BGP's connection buffer is filled this way, so every received message
looked empty (`t/56`, `t/90`, `t/91` hung or failed).
