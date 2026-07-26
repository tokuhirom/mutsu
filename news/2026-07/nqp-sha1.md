# `nqp::sha1`, the one nqp op two shipped components actually need

mutsu had no SHA-1 anywhere: nothing in tree, and no `sha`/`digest` crate in
`Cargo.toml`. That made `nqp::sha1` the single unimplemented `nqp::` op with
real demand inside our own tree — a demand measured, not guessed, while deciding
whether to build an `nqp::` op layer at all (the answer was no; see
`news/2026-07/nqp-op-layer-measured-and-rejected.md`):

- **vendored zef** — `Zef::Distribution.id` is `nqp::sha1(self.Str)`, and
  `Zef::CLI`'s `locate` derives installed-source paths from it. That is the mzef
  critical path.
- **bundled `modules/OpenSSL`** — `dll-resource()` hashes a resource name to
  find its unmangled copy under `$*TMPDIR`. `use OpenSSL` worked only because
  that sub is never called at load time.

Both would have failed with `Unsupported nqp:: op: nqp::sha1` — the error
introduced the same day, which replaced a worse outcome: before it, an
unimplemented `nqp::` op fell through the package-prefix strip in
`call_function_fallback` and quietly reached Raku's same-named builtin.

## What landed

`src/builtins/sha1.rs` implements FIPS 180-4 SHA-1 in ~90 lines with no new
dependency. A crate would have meant an optional-dependency matrix across the
`native`/`wasm` feature split for one fixed, tiny algorithm. `sha1_digest`
returns the 20 raw bytes; `sha1_hex_uppercase` formats the 40 uppercase hex
digits nqp's op yields.

The op itself is one arm beside `nqp::ordat` / `nqp::gethostname` /
`nqp::bindattr` in `src/runtime/builtins.rs`, matched under its full `nqp::`
name — so it is reached before the guard that rejects unimplemented ops.

The digest covers the string's **UTF-8 encoding**, not its codepoints, which is
what rakudo does: `nqp::sha1("日本語")` is
`C12140A0FFB4E56481B4FE0A7A25040C2EAFA9CA`, the same as `sha1sum` over those
bytes.

## Verification

Four Rust unit tests carry the NIST vectors (`""`, `"abc"`, the 448-bit message,
a million `a`), the padding block boundaries (55 / 56 / 64 bytes — where the
length field does and does not fit in the first block), and the UTF-8 property.

`t/nqp-sha1.t` (12 subtests) pins the op end to end and **passes unchanged under
rakudo**, so it is a differential test rather than a record of mutsu's own
output.

The real consumer was checked directly: building a `Zef::Distribution` and
asking for its `.id` gives byte-identical results under both implementations.

```
$ mutsu -I vendor/zef/lib tmp/zef-dist-id.raku
Foo::Bar:ver<1.2.3>:auth<zef:someone>:api<1>
8BB1CD1A566589A4514F3C562490D45DB20BDBAB
$ raku  -I vendor/zef/lib tmp/zef-dist-id.raku      # identical
```
