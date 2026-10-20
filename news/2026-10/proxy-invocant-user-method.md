# A method called on a Proxy honours the fetched value's own methods

A method call on a Proxy, such as the result of an `is rw` accessor that
returns one, decides whether a user method shadows the built-in row before it
fetches the Proxy. So it asked that question about the Proxy, not about the
value it holds. A class that `does Blob[uint8]` and declares its own `Str`
therefore had `.Str` answered by `Blob.Str`, which dies with "Stringification
of a ... is not done with 'Str'". Without the Proxy, the same call used the
class's own method. The shadow query now runs again on the fetched value.

PDF's `PDF::IO::Blob` is such a class, and `$stream.encoded` is a Proxy, so
`t/indobj-stream.t` died at `$stream-obj.encoded.Str` (#12907).
