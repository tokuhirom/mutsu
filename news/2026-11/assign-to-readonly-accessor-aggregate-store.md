# Assigning to a read-only accessor that holds an Array, Hash or Buf stores into it

`self.buffer = buf8.new` on a `has buf8:D $.buffer` (no `is rw`) died
`X::Assignment::RO`. The accessor hands back the stored object and `=` on an
object that has a STORE replaces its contents in place (rakudo's `infix:<=>`),
which already worked for `Array` and `Hash` values only when held by a `@`/`%`
attribute. A `$`-sigiled non-`rw` attribute holding an `Array`, `Hash` or `Buf`
now takes the assignment through the same in-place store, for `self.attr = v`,
`$.attr = v` and `$obj.attr = v`; a `Buf` target accepts a `Buf`, a list or one
element as `Buf.STORE` does. An `Int` attribute is still read-only.
Net::BGP's `Connection.pop-bgp-message` clears its buffer this way.
