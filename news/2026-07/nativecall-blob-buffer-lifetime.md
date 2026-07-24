# A NativeCall Blob buffer now lives as long as the Raku object

`OpenSSL`'s `03-rsa` test file segfaulted mutsu on its second assertion. The
root cause was a lifetime, not a crash bug: a `Blob` argument was copied into a
`Vec<u8>` that only lived for the duration of the call, and
`BIO_new_mem_buf(buf, len)` builds a read-only memory BIO **over the caller's
memory** and reads it later. The BIO was left pointing at freed memory, so
`PEM_read_bio_RSAPrivateKey` returned NULL and `RSA_size(NULL)` took the process
down. The module even documents the requirement — "`$bio-buf` contains a C
pointer to `$private-pem-buf`, so `$private-pem-buf` needs to stay alive as long
as `$bio-buf` does" — which Rakudo satisfies for free, because there a `Blob`'s
storage *is* the C buffer.

mutsu stores a `Buf`'s bytes as an `Array` of boxed `Int`s, so there is no
contiguous storage to hand to C directly. The fix pins the C-side copy to the
Raku object instead of to the call: `runtime::nativecall_pin` keeps the bytes in
a registry keyed by the object's attribute-cell address, the first call
allocates and every later call refreshes that same allocation, and the entry is
released from `Drop for InstanceAttrs`. So the C buffer lives exactly as long as
the `Buf` that owns it — no leak, and no dangling pointer while the object is
still reachable. Release short-circuits on a relaxed atomic load, so a program
that pins nothing pays one read per instance drop. (The real fix remains giving
`Buf`/`Blob` a native contiguous representation, and a `TODO` in the new module
says so.)

Two further gaps surfaced on the way to a passing `03-rsa`:

- An **unparameterized `CArray` parameter** was mis-registered. OpenSSL declares
  `RSA_sign(int32, Blob, int32, Blob, CArray, OpaquePointer)`, whose fifth
  argument is the `unsigned int*` output length. `CArray` has no mapped C type
  and starts with a capital letter, so it fell through to the CStruct branch and
  was passed as an opaque `void*` — i.e. NULL, since a `CArray` carries no
  address — and `RSA_sign` wrote through it. A bare `CArray` is now marshalled
  as a C buffer whose element type is read from the argument, which
  `CArray[T].new` tags with its element type.
- A **NULL pointer return** produced a defined `Pointer` holding 0 rather than
  the type object, so the caller's usual `die "..." unless defined($p)` guard
  never fired. It now returns the type object, as raku does.

`OpenSSL 03-rsa` goes from a segfault at test 2 to 8/8; the bundled-library
baseline is 14/18. Pinned by `t/nativecall-buf-lifetime.t` and
`t/nativecall-bare-carray-param.t`, both of which also pass unchanged under
`raku`.
