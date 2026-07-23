# Battery: TLS / HTTPS socket — `OpenSSL` + `IO::Socket::SSL`

**Slot:** TLS / HTTPS socket (foundation) · **Providers:** `OpenSSL`
(`auth<zef:raku-community-modules>`, v0.2.7) + `IO::Socket::SSL`
(`auth<raku-community-modules>`, v0.0.4) · **Kind:** Adopted (community modules,
vendored as-is) · **License:** MIT / MIT

This is the **first active battery target** and, by design, a **NativeCall
campaign** rather than a quick win. It is the foundation the whole pure-Raku HTTP
stack stands on, so it is sequenced first.

## Status: working (end-to-end HTTPS)

Both modules are vendored into the bundled `modules/` tree and resolve with
**zero config** (`use OpenSSL;` / `use IO::Socket::SSL;` — no `-I`). The genuine
community binding runs on mutsu far enough to complete a **real `https://` GET**:

```raku
use IO::Socket::SSL;
my $sock = IO::Socket::SSL.new(:host<example.com>, :port(443));
$sock.print("GET / HTTP/1.0\r\nHost: example.com\r\n\r\n");
say $sock.recv;   # HTTP/1.1 200 OK ...
```

The interpreter grew the NativeCall + parser features the binding needs (all
general improvements, none patched into the vendored source — rung 2 of the
[adoption policy](../../BATTERIES.md#1-adoption-policy--community-first-adopt-as-is)):

- **`is native(&sub)`** — the library name is produced by calling a Raku sub at
  bind time (`is native(&ssl-lib)`); the trait argument is now invoked when it is
  a code object.
- **`is repr('CStruct')` opaque handles** — `SSL` / `SSL_CTX` / `SSL_METHOD` /
  `BIO` / … are marshalled **by pointer**: a CStruct return is wrapped in a
  defined instance of the declared class (carrying the C address); passing one
  back reads that address. Registered CStruct classes are tracked so even a
  lowercase class name (`evp_cipher_st`) is recognized.
- **`Blob`/`Buf` byte buffers** — passed as a `void*` to their bytes, with the
  callee's writes copied back (the `SSL_read` / `BIO_read` out-buffers).
- **Qualified-name native dispatch** — an `our sub` in a `unit module` called by
  its package-qualified name (`OpenSSL::EVP::EVP_aes_128_cbc`) reaches the native
  descriptor.
- **`IO::Socket` role** — a minimal built-in role so `class IO::Socket::SSL does
  IO::Socket` composes; the native `IO::Socket::INET` satisfies the role for the
  `set-socket(IO::Socket $s)` type check. `explicitly-manage` is a no-op.
- **Parser fix** — `v4-split` / `v6-split` are ordinary identifiers, not the
  version literal `v4` followed by `-split` (a too-greedy version lexer broke the
  binding). A `-` is only a version character as a trailing minus marker
  (`v1.2.3-`), not before a word char.

Pinned by `t/openssl-battery.t` (offline load + client context, skips if the host
lacks `libssl`), `t/nativecall-native-lib-sub.t`, and `t/version-vn-identifier.t`.
Remaining follow-ups (not on the HTTPS path): NativeCall callbacks (verify
callbacks) and by-value CStruct field marshalling.

## Why this is first

The chosen HTTP client direction is the mature **`HTTP::UserAgent`** (see
[http-client.md](http-client.md)), which **hard-depends** on `IO::Socket::SSL`.
And every pure-Raku HTTP client (`HTTP::UserAgent` and `HTTP::Tiny` alike)
delegates HTTPS to `IO::Socket::SSL` → the `OpenSSL` binding. So `OpenSSL` is the
critical-path foundation: nothing above it (HTTPS for any client, plus the OpenSSL
digest/crypto modules) works until it runs on mutsu. We build up from the bottom.

Growing mutsu's NativeCall to run the genuine `OpenSSL` binding is **aligned
investment we want regardless** — NativeCall is a first-class subsystem we intend
to build out, not a subsystem we are reluctantly touching for TLS. (User decision,
2026-07-24.)

## Dependency + license facts

```
IO::Socket::SSL v0.0.4  (MIT)
└─ OpenSSL v0.2.7        (MIT, zero Raku deps; native dep on system libssl/libcrypto)
```

Both MIT — clears the [license gate](../../BATTERIES.md#4-license-policy). `OpenSSL`
provides 19 modules (`OpenSSL::SSL`, `::Ctx`, `::Bio`, `::X509`, `::EVP`, digests,
RSA, …); the HTTPS path needs the SSL/Ctx/Bio/Method/Err/X509 subset.

## What mutsu must grow to run the real `OpenSSL` binding

Measured against the vendored `OpenSSL` 0.2.7 source. This is the concrete gap
list — the campaign's work items, roughly in load order:

1. **`%?RESOURCES` + resource JSON (first blocker).** `use OpenSSL::SSL` fails at
   `CHECK` today: `OpenSSL::NativeLib` does
   `BEGIN Rakudo::Internals::JSON.from-json: %?RESOURCES<libraries.json>.slurp` to
   learn the platform library names. Needs `%?RESOURCES<...>` resolving a dist
   resource, `Rakudo::Internals::JSON.from-json`, and `$*VM.platform-library-name`
   (which turns `ssl`/`crypto` into `libssl.so.3` / `libcrypto.so.3` on Linux).
2. **`is native(&lib)`** — the native library name is supplied dynamically by
   *calling a Raku sub at bind time* (`is native(&ssl-lib)`), not a string
   literal. mutsu's `is native` must accept a code-block argument.
3. **`is repr('CStruct')` structs — 14 of them** (SSL, SSL_CTX, BIO, X509, EVP,
   Cipher, Session, Stack, Method, …). Simple integer-field CStructs already
   construct on mutsu; the remaining work is the nested / pointer-carrying structs
   that get passed to and returned from native calls (PLAN.md §1 B4 ② NativeCall
   remainder).
4. **CArray / buffer round-trips** for `SSL_read` / `SSL_write` (read into a
   caller-provided byte buffer; write a `Blob`), including a *returned* pointer
   reified back to a Raku buffer.
5. **Handshake control flow** — `SSL_new` / `SSL_set_fd` / `SSL_connect` /
   `SSL_get_error` retry loop over a connected `IO::Socket::INET` fd, then
   `IO::Socket::SSL`'s Raku layer on top.

Callbacks (verify callbacks) are **not** required for a basic TLS client, so the
NativeCall callback gap (B4 ③) is out of scope for the first HTTPS milestone.

## Packaging impact (must land with this track)

`OpenSSL` `dlopen`s the **system** OpenSSL at runtime (it resolves
`libssl.so.3` / `libcrypto.so.3` via `$*VM.platform-library-name`), so bundling it
adds a runtime packaging requirement that must land **in the same PR as this
track**:

- **`Dockerfile`**: add `libssl3` to the runtime-stage `apt-get install`
  (`ca-certificates` is already present). Builder stage unchanged.
- **Release tarball**: document "system libssl required for HTTPS" (present by
  default on Linux/macOS).

## Security updates

This slot is the strongest case for the batteries
[independent-update policy](../../BATTERIES.md#6-security-updates-and-independent-updatability),
and it retroactively justifies binding the system OpenSSL over statically linking
a vendored TLS implementation:

- **The TLS crypto itself (`libssl`/`libcrypto`) rides the OS.** mutsu `dlopen`s
  the host library, so TLS CVEs are patched by the OS package manager
  (`apt upgrade libssl3`) **without any mutsu release**. A statically-linked TLS
  stack would have forced a full mutsu rebuild + redistribution for every OpenSSL
  CVE — unacceptable for the most security-critical dependency.
- **The Raku bindings (`OpenSSL`, `IO::Socket::SSL`) override via `mzef`.** A
  patched binding version installed with `mzef install` shadows the bundled copy
  (the bundle is the lowest-priority source), so users are never blocked on our
  release cadence for a binding-level fix either.

## First implementation slice

A bounded first PR that de-risks the campaign:

1. Vendor `OpenSSL` (+ its `resources/`, `LICENSE`, `META6.json`) into the bundled
   `modules/` tree per the [vendoring policy](../../BATTERIES.md#3-vendoring-and-resolution).
2. Get `use OpenSSL::SSL` **past `CHECK`** — close gap (1) (`%?RESOURCES` +
   `$*VM.platform-library-name` + `Rakudo::Internals::JSON`). Success criterion:
   the module loads and `ssl-lib()` returns `libssl.so.3` without error.
3. Land `t/` pins for whatever general fixes fall out (resource resolution,
   `$*VM.platform-library-name`), then continue with gaps (2)→(5) in subsequent
   slices, ending in an end-to-end `IO::Socket::SSL` connect + a real `https://`
   smoke test.

## Provenance and update procedure

Per [BATTERIES.md §3](../../BATTERIES.md#updating-a-vendored-module-must-be-documented-per-library).
To bump either module to a new upstream release (e.g. a security fix), re-vendor —
do **not** hand-edit the vendored tree:

| Module | Upstream | Pinned version |
| --- | --- | --- |
| `OpenSSL` | <https://github.com/raku-community-modules/OpenSSL> | v0.2.7 |
| `IO::Socket::SSL` | <https://github.com/raku-community-modules/IO-Socket-SSL> | v0.0.4 |

```sh
# 1. Get the new upstream release into a checkout, then rsync it into modules/.
#    Keep runtime files + attribution; drop upstream tests/CI/precomp, the doc
#    tree, and the Windows-only resource DLLs (mutsu has no Windows target, so
#    the 2.8 MB `libeay32.dll`/`ssleay32.dll` would be dead weight in git).
rsync -a --delete \
  --exclude='.git' --exclude='.github' --exclude='t/' --exclude='xt/' \
  --exclude='doc/' --exclude='run-tests' --exclude='dist.ini' \
  --exclude='Build.rakumod' --exclude='*.precomp' --exclude='.precomp/' \
  --exclude='resources/*.dll' \
  <openssl-checkout>/ modules/OpenSSL/
# (same for IO-Socket-SSL -> modules/IO-Socket-SSL/)
```

**Two vendoring notes specific to OpenSSL:**

- **`resources/libraries.json` is generated, not shipped upstream.** Upstream's
  `Build.rakumod` writes it at install time (`{ "crypto": "crypto", "ssl": "ssl" }`
  on a non-brew Linux/macOS host, which `$*VM.platform-library-name` turns into
  `libssl.so.3` / `libcrypto.so.3`). Since we drop `Build.rakumod`, commit that
  file directly. If a future upstream changes the library stems, regenerate it.
- **Windows DLLs are excluded** (see the `--exclude='resources/*.dll'` above).
  They are referenced by `META6.json`'s `resources` list but are only used on
  Windows, which mutsu does not target; the Linux/macOS path only reads
  `libraries.json` and `dlopen`s the system `libssl`.

2. Bump the version/commit in the table above and in the
   [bundle index](../../BATTERIES.md#7-bundle-index).
3. **Verify:** `use OpenSSL::SSL` loads, and the `https://` smoke test
   (`t/`) still passes.

Note: a *deployed* mutsu can also take a patched binding without a re-vendor —
`mzef install OpenSSL` / `mzef install IO::Socket::SSL` shadows the bundled copy
(see [Security updates](#security-updates)). Re-vendoring is for the next release,
so fresh installs ship the fix too.

## License

- `OpenSSL` — MIT. Upstream: <https://github.com/raku-community-modules/OpenSSL>.
- `IO::Socket::SSL` — MIT. Upstream:
  <https://github.com/raku-community-modules/IO-Socket-SSL>.
- Vendored verbatim with `LICENSE` / `META6.json` / `README` preserved; sources
  unmodified (per [BATTERIES.md §4](../../BATTERIES.md#4-license-policy)).
