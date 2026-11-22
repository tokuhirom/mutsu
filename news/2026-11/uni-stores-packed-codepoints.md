# A `Uni` stores its codepoints as a packed `uint32` buffer

A fresh profile of the 100-record SPDX decode (`bench-json-fast-spdx`, #8673)
put more than half of a record in JSON::Fast's non-word string path:
`parse-string-slow` turns the string into a `Uni` with `nqp::strtocodes`, and
`unjsonify-string` moves it to a second `Uni` one `nqp::shift_i` and one
`nqp::push_i` per character. That loop cost about 870 instructions per
character, and about 400 of them were the two list ops and `nqp::elems`.

The cause was the representation. `UniData` kept its codepoints as an `Array`
of boxed `Value`s, so every codepoint paid a NaN-box encode on the way in, a
decode and a drop on the way out, two view decodes (the `Uni`, then its
array), and the general `ArrayData` editor's bookkeeping.

rakudo declares `Uni` as a `uint32` VMArray. mutsu now stores it as one:
`UniCodes`, a `Vec<u32>` with a start offset (so `shift_i` is O(1), as on
MoarVM), in its own `Gc` node so that every holder of a `Uni` still sees an
edit made through another.

- `shift_i`, `push_i`, `pop_i`, `unshift_i`, `elems`, `atpos_i`, `bindpos_i`
  and `setelems` reach the store directly, in TRIR and on the generic path.
- `strtocodes` fills the store and `strfromcodes` reads it in place.
- `splice` and `slice` edit it as elements and write it back, as a Buf's
  bytes are.

Two wrong answers went with it: `nqp::unshift_i` on a `Uni` died with
"expected a Buf/Blob or array", and an int pushed onto a `Uni` kept its sign
(`-1` read back as `-1`, not rakudo's `4294967295`).

Measured with callgrind on a profiling build, one 100-record decode (two
decodes minus one, same process): **71.13 M → 62.25 M instructions (-12.5%)**.
This is one slice of #8673; the row is still behind rakudo.
