# OpenSSL / IO::Socket::SSL battery: real HTTPS runs on mutsu

The TLS foundation of the [batteries](../../BATTERIES.md) effort is working. The
genuine community `OpenSSL` (v0.2.7) and `IO::Socket::SSL` (v0.0.4) modules are
vendored into the bundled `modules/` tree and run on mutsu far enough to complete
an actual `https://` request:

```raku
use IO::Socket::SSL;
my $sock = IO::Socket::SSL.new(:host<example.com>, :port(443));
$sock.print("GET / HTTP/1.0\r\nHost: example.com\r\n\r\n");
say $sock.recv;   # HTTP/1.1 200 OK ...
```

`use OpenSSL` / `use IO::Socket::SSL` resolve with **zero config** — no `-I`. The
bundle is registered as the lowest-priority module source (searched after every
`-I`/`MUTSULIB`/project-local/`mzef`-site path), which is both the "floor" and
the independent-update mechanism: an `mzef install`ed newer version shadows the
bundled copy. The security-critical crypto is the **system** `libssl` (mutsu
`dlopen`s it), so its CVEs are patched by the OS, not a mutsu release.

Per the adoption policy, nothing was patched into the vendored modules — the
interpreter grew the general NativeCall + parser features the real binding needs:

- **`is native(&sub)`**: the native library name supplied by a code object
  (`is native(&ssl-lib)`) is resolved by calling the sub at bind time.
- **`is repr('CStruct')` opaque handles**: `SSL` / `SSL_CTX` / `SSL_METHOD` /
  `BIO` and friends are marshalled by pointer — a CStruct return becomes a
  defined instance of the declared class carrying the C address, and passing one
  back reads that address. Declared CStruct classes are tracked so a lowercase
  name (`evp_cipher_st`) is recognized too.
- **`Blob`/`Buf` buffers**: passed as a pointer to their bytes, with the callee's
  writes copied back — the `SSL_read` / `BIO_read` out-buffers.
- **Qualified-name native dispatch**: an `our sub` in a `unit module` invoked by
  its package-qualified name reaches the native descriptor (short-name fallback).
- **`IO::Socket` role**: a minimal built-in role so `class IO::Socket::SSL does
  IO::Socket` composes, plus role membership for the native `IO::Socket::INET`;
  `explicitly-manage` is a no-op (mutsu keeps `Str` args alive for the call).
- **Version-lexer fix**: `v4-split` / `v6-split` are identifiers, not the version
  literal `v4` followed by `-split`. The rule: a `-` is a version character only
  as a trailing *minus marker* (`v1.2.3-`), i.e. when it is NOT followed by a word
  char; a `-`/`'` before a word char is an identifier joiner. Both the identifier
  form and the trailing-marker form (`roast/S02-literals/version.t`) now parse.

Packaging: the release tarball and the container now ship the modules at
`share/mutsu/modules` (resolved exe-relative, or via `MUTSU_BUNDLE_DIR`), and the
runtime container installs `libssl3`.

Pins: `t/openssl-battery.t` (offline zero-config load + client context; skips if
the host has no `libssl`), `t/nativecall-native-lib-sub.t` (the NativeCall
features against libc), and `t/version-vn-identifier.t` (the version/identifier
boundary).
