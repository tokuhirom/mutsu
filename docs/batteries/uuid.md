# Battery: UUID — `UUID`

**Slot:** UUID generation · **Chosen:** `UUID` (`auth<github:retupmoca>`,
v1.0.0, MIT) · **Kind:** Adopted (community module, vendored as-is)

## What it is

Generates a random (version 4) UUID and renders it in canonical
`8-4-4-4-12` hex form:

```raku
use UUID;

my $u = UUID.new(:version(4));
say $u.Str;    # e.g. "d3b4ba3c-e512-4936-8251-02a383b0ba63"
say $u.Blob;   # the underlying 16-byte Buf
```

Single file (33 lines), zero dependencies.

## Field surveyed

Per [selection-method.md](selection-method.md), the field enumerated from
`~/.zef/store/rea/rea.json` for the `uuid` keyword:

| Candidate | Auth | Version / released | License | Deps | Dependents | Notes |
| --- | --- | --- | --- | --- | --- | --- |
| **`UUID`** | `github:retupmoca` | 1.0.0 / 2018-04-30 | MIT (in `LICENSE`, not declared in `META6.json`) | 0 | **30** (`BSON`, `Air`, `Auth::SAML2`, `DateTime::US`, ...) | Chosen |
| `UUID::V4` | `zef:masukomi` | 1.0.0 / 2022-10-01 | MIT | 1 (`Crypt::Random`, already bundled) | 0 | Newest release, but zero ecosystem uptake and only covers v4 generation (a strict subset of `UUID`'s already-narrow scope) |
| `LibUUID` | `cpan:CTILMES` / `github:CurtTilmes` | 0.5 / 2019-04-05 | Artistic-2.0 | `NativeCall`, `NativeLibs` | 3 (`DB::Pg`, `Marrow`, `Mint`) | NativeCall bindings to the system `libuuid` — needs a runtime C library dependency mutsu does not otherwise require for this slot, for a capability the zero-dep `UUID` already covers |

`UUID` wins decisively on the dependents count (§2's "ecosystem standing"
criterion) and has zero dependencies, so it was the clear pick without
needing to weigh the license/deps trade-offs further. `UUID::V4`'s only
substantive difference (depending on the already-bundled `Crypt::Random` for
cryptographic randomness rather than `roll`) does not matter enough on its
own to override 30 real dependents choosing the other module. `LibUUID`
was rejected because it trades a zero-dependency pure-Raku module for a
NativeCall binding to a system library, for the same random-v4-UUID
capability `UUID` already provides — not worth the added runtime dependency
for this slot.

**Maintenance note:** `UUID` was last released in 2018 (no repo tags at all
— the vendored commit is simply `master`'s tip) and ships only one test
(`t/01-basic.t`, an `ok $u` truthiness check). This is thin, but the module
itself is 33 lines and does exactly one thing; a thin test still exercises
`.new`/`.Str`/`.Blob`, which is enough to gate a regression (per
selection-method.md §0, a zero-test candidate is disqualified — this one
clears that bar, barely).

## Interpreter work it drove (rung 2 — grow mutsu, never patch the module)

**A user class's own `has $.bytes` accessor was shadowed by a target-agnostic
native `.bytes` builtin.** `UUID`'s only attribute is `has $.bytes` (holding
the 16-byte `buf8`); reading it through `.bytes` returned the *stringified
object's character count* instead of the accessor's stored value:

```raku
class Foo { has $.bytes; }
my $u = Foo.new(:bytes(buf8.new(1,2,3,4,5)));
say $u.bytes;   # mutsu (before fix): 5   (== "Foo()".chars)
                # raku:               Buf[uint8]:0x<01 02 03 04 05>
```

Root cause: `.bytes` is a *Cool-only* builtin in real Rakudo — a plain
`Any`-derived class does not resolve it at all (`No such method 'bytes' for
invocant of type 'Foo'`), exactly like `.uc`/`.chars`/`.flip`/etc (ADR-0051
P4). Those Cool-only names already defer to the interpreter (which then
correctly prefers a class's own accessor, or throws if there is none) via
`cool_only_builtin_method()` in `src/runtime/methods_native_bypass.rs` —
**but `bytes` was missing from that list**, so it kept taking the
target-agnostic native fast path (`dispatch_core_unicode.rs`'s `_ =>
Value::int(target.to_string_value().len())` catch-all) unconditionally,
even for a receiver whose own class declares a public `bytes` accessor.

Fix: add `"bytes"` to `cool_only_builtin_method()`'s match arms, right next
to `"chars"`/`"codes"` (the same string-length family, and already correctly
gated). This is a **general** dispatch-priority fix, not specific to `UUID`
or to `Buf`/`Blob` — any user class declaring `has $.bytes` was affected.
Real `Buf`/`Blob.bytes` is unaffected: their native-row catalog entry
(`("Blob", "bytes", 1, 8)` in `native_method_row_table.rs`) is found by the
same `e2_native_method_exists` check the other Cool-only names already rely
on, so the native fast path still answers for genuine `Buf`/`Blob` receivers.
Pin: `t/bytes-attribute-accessor-not-shadowed.t`.

Upstream test: 1 file, 1 subtest — passes under mutsu, matching raku. Smoke:
`t/uuid-battery.t`.

## Provenance and update procedure

Per [BATTERIES.md §3](../../BATTERIES.md#updating-a-vendored-module-must-be-documented-per-library).
To bump the module, re-vendor — do **not** hand-edit the vendored tree:

| Module | Upstream | Pinned version | Commit |
| --- | --- | --- | --- |
| `UUID` | <https://github.com/retupmoca/P6-UUID> | master tip (no tags) | `26cb8696` (2018, last commit on the repo) |

What is vendored: `lib/` plus `META6.json`, `LICENSE`, `README.md` for
attribution. Upstream `t/` and CI config are excluded — the release gate
fetches the tests fresh at the pinned commit.

```sh
rsync -a --exclude '.precomp' <checkout>/lib/ modules/UUID/lib/
cp <checkout>/{META6.json,LICENSE,README.md} modules/UUID/
# then bump batteries.lock, re-run the gate, refresh the Pages manifest:
cargo build --release && scripts/battery-testsuite.sh --update
git diff batteries-whitelist.txt
python3 scripts/gen-batteries-manifest.py
```

Verification after a bump:

```sh
mutsu -e 'use UUID; say UUID.new(:version(4)).Str'
```

## License

**MIT** — stated in the shipped `LICENSE` file (Copyright (c) 2015 Andrew
Egeler); `META6.json` does not carry a `license` field, but §2's rule to
cross-check against the shipped `LICENSE`/`LICENCE` applies, and there is no
ambiguity here (one license file, one clear grant). Vendored verbatim with
`LICENSE` / `META6.json` / `README` preserved for attribution, source
unmodified (per
[BATTERIES.md §4](../../BATTERIES.md#4-license-policy)).
