# Battery: Cryptographic digests — `Digest`

**Slot:** Message digests (MD5 / SHA-1 / SHA-2 / SHA-3 / RIPEMD-160 / HMAC) ·
**Chosen:** `Digest` (`auth<zef:grondilu>`, v1.1.0, Artistic-2.0) ·
**Kind:** Adopted (community module, vendored as-is)

## What it is

Six modules, one dist, no dependencies and no native code:

```raku
use Digest::MD5;     say md5("abc").list».fmt("%02x").join;      # 900150983cd24fb0…
use Digest::SHA1;    say sha1("abc").list».fmt("%02x").join;     # a9993e364706816a…
use Digest::SHA2;    say sha256("abc").list».fmt("%02x").join;   # ba7816bf8f01cfea…
                     # also sha224, sha384, sha512
use Digest::SHA3;    say sha3_256("abc").list».fmt("%02x").join; # 3a985da74fe225b2…
                     # also sha3_224/384/512 and the SHAKE XOFs
                     # (`shake256("x", 16)`, or `*` for an endless block stream)
use Digest::RIPEMD;  say rmd160("abc").list».fmt("%02x").join;   # 8eb208f7e05d987a…
use HMAC;            say hmac(key => "Jefe", msg => "…", hash => &sha1, block-size => 64);
```

Every digest takes a `Str` or a `Blob` and returns a `Blob`. `HMAC` is generic
over any of them (RFC 2104), so the dist covers HMAC-SHA-256 and friends with
nothing else installed.

## Why it is bundled

**Digests are table stakes for the "small web blog with the bundle alone"
yardstick** (BATTERIES.md §2): session ids, ETags, token hashing,
API request signing, cache keys and file checksums all need one, and today a
mutsu user has to `mzef install` something to get an MD5. It is also the most
depended-upon crypto primitive in the ecosystem.

**Selection — a measured survey** (procedure:
[selection-method.md](selection-method.md); field enumerated from the Zef index
snapshot at `~/.zef/store/META.json`, 14787 dist releases, 2026-08-04).
Reverse-dependency counts (distinct dists naming it in `depends`):

| Candidate | Rdeps | Deps | Native? | License | Verdict |
| --- | --- | --- | --- | --- | --- |
| **`Digest` (zef:grondilu)** | **26** | none | pure Raku | Artistic-2.0 | **chosen** |
| `Digest::HMAC` (zef:jjmerelo) | 22 | none (test-only: `Digest`) | pure Raku | MIT | loses *this* slot, but bundled in its own — see below |
| `Digest::SHA` | 15 | — | — | — | **does not exist** in the index; its 15 dependents are unresolvable |
| `Digest::SHA1::Native` (zef:bduggan) | 10 | `LibraryMake` | C, built at install | Artistic-2.0 | rejected — needs a compiler |
| `Digest::SHA256::Native` (zef:bduggan) | 10 | `LibraryMake` | C, built at install | Artistic-2.0 | rejected — needs a compiler |
| `Digest::MD5` (github:cosimo) | 7 | none | pure Raku | *none stated* | rejected — MD5 only, no license |
| `OpenSSL::Digest` (already bundled) | — | system `libssl` | NativeCall | MIT | rejected as *the* slot — see below |
| `Digest::xxHash`, `Digest::MurmurHash3`, `Digest::FNV` | ≤1 | — | — | — | not cryptographic; different job |

Why each loser lost, in the terms of BATTERIES.md §2:

- **`Digest::SHA1::Native` / `Digest::SHA256::Native`** are the fastest option
  and have real ecosystem weight, but they depend on `LibraryMake`: the dist
  compiles a C shared library **at install time**. A bundled battery must work
  from an unpacked tarball with no toolchain, so a build step disqualifies them
  outright. They are also single-algorithm — bundling both still leaves no MD5,
  SHA-3 or RIPEMD.
- **`OpenSSL::Digest`** would give every algorithm for free, since `OpenSSL` is
  already bundled for TLS. It loses the *slot* on two counts. It needs the host
  `libssl`/`libcrypto` at runtime, so `md5("x")` would stop being a
  zero-configuration operation on a machine without it (TLS legitimately
  requires the system library; hashing a string does not). And `md5`/`sha1`
  there are `my proto sub`s, which a `&hash` callback parameter still mis-binds
  under mutsu (see `todo/tickets/digest-dist-blockers.md`), so it is not a drop-in
  for the HMAC use case. It remains available and is the right choice when
  throughput matters — upstream's own README says as much ("if you need a faster
  way to compute digest, consider using a nativecall binding to the OpenSSL
  library instead"), which is an argument about speed, not about which one
  belongs in a zero-configuration bundle.
- **`Digest::HMAC` (jjmerelo)** is a 23-line wrapper that loses *this* slot: it
  supplies no hash function of its own — `hmac($key, $msg, &hash)` takes the
  digest as a callback — so it cannot fill a message-digest slot, and this
  dist's own `HMAC` module already provides an `hmac`.

  **It is bundled too, since 2026-08-08** — see
  [digest-hmac.md](digest-hmac.md). It is a hard dependency of `JSON::JWT`, and
  therefore of Cro's WebToken auth, which could not even *load* without it. The
  two batteries are complementary, not alternatives: this dist is the hash
  functions, `Digest::HMAC` is the RFC 2104 construction over any of them (and
  the name 22 ecosystem dists actually `depends` on).

  **Correction to this survey:** the original entry rejected it for stating "no
  license anywhere". That was wrong — upstream ships an MIT `LICENSE`
  (`Copyright (c) 2014 Andrew Egeler`); only its `META6.json` omits a `license`
  key, which is what the index-based survey saw. BATTERIES.md §4 requires the
  license to be permissive, preserved and recorded, not declared in the META, so
  the gate it was said to fail never applied.
- **`Digest::MD5` (cosimo)** is MD5-only and states no license.

**Proven behaviour on mutsu.** This is the criterion that took the work: the
dist is dense, idiomatic Raku over native buffers, and running it correctly
drove five rounds of general interpreter fixes — well over twenty — see
`news/2026-08/digest-dist-seven-fixes.md`,
`news/2026-08/digest-dist-followup-four-fixes.md`,
`news/2026-08/digest-md5-four-fixes.md`, `news/2026-08/digest-sha3-runs.md`,
`news/2026-08/state-vars-belong-to-the-block-clone.md`,
`news/2026-08/begin-in-a-module-routine-runs-once.md` and
`news/2026-08/native-array-push-after-a-start.md`. Not one of them is
Digest-specific; each is pinned by its own `t/` file. Making it run *was* the
compatibility win (BATTERIES.md rung 2).

**Zero dependencies, one directory, 6 files, ~460 lines.** Nothing to vendor
transitively.

## Test-suite gate

Upstream has 4 test files; 3 are whitelisted and run on every release:

| file | what it covers | status |
| --- | --- | --- |
| `t/md5.t` | RFC 1321 vectors + 100 random strings | **PASS** (2.2s) |
| `t/sha.t` | SHA-1, all four SHA-2 widths, all four SHA-3 widths | **PASS** (1.5s) |
| `t/rfc4231.t` | the RFC 4231 HMAC-SHA-2 test vectors | **PASS** (5.9s) |
| `t/ripemd.t` | RIPEMD-160, incl. the 1,000,000-byte `'a' x 1e6` vector | not whitelisted — **correct but slow** |

`t/ripemd.t` produces the right digest for all 9 vectors; it took ~513s
against raku's ~46s when first measured, over the gate's 120s per-file
budget. The cost is structural, not a wrong answer: `rmd160` runs the two
halves of each compression round in `start` blocks, so a 1 MB message
spawns ~31k tasks. Successive campaigns (worker pool ADR-0020, per-task
clone slimming, closure-setup allocations #5941, reduce compiled-first
dispatch #5942) brought it to **~119s local (2026-08-05)** — right at the
budget line, but the gate is a hard `timeout 120`, so it stays
un-whitelisted until one more lever gives real margin on slower CI
runners. Tracked in
`todo/perf/digest-ripemd-start-per-block-overhead.md`.

## Provenance and update procedure

Per [BATTERIES.md §3](../../BATTERIES.md#updating-a-vendored-module-must-be-documented-per-library).
To bump the module, re-vendor — do **not** hand-edit the vendored tree:

| Module | Upstream | Pinned version | Commit |
| --- | --- | --- | --- |
| `Digest` | <https://github.com/grondilu/libdigest-raku> | v1.1.0 | `2870e658` (2025-02-28) |

What is vendored: `lib/` plus `META6.json`, `LICENSE`, `README.md` for
attribution. Upstream `t/` is excluded — the release gate fetches the tests
fresh at the pinned commit.

```sh
rsync -a --exclude '.precomp' <checkout>/lib/ modules/Digest/lib/
cp <checkout>/{META6.json,LICENSE,README.md} modules/Digest/
# then bump batteries.lock, re-run the gate, refresh the Pages manifest:
cargo build --release && scripts/battery-testsuite.sh --update
git diff batteries-whitelist.txt
python3 scripts/gen-batteries-manifest.py
```

Verification after a bump:

```sh
mutsu -e 'use Digest::MD5;  say md5("abc").list».fmt("%02x").join'
# 900150983cd24fb0d6963f7d28e17f72
mutsu -e 'use Digest::SHA2; say sha256("abc").list».fmt("%02x").join'
# ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad
```

Updates ride the `mzef` layer (BATTERIES.md §6): the bundled tree is the
lowest-priority source, so `mzef install Digest` shadows it with a newer
version without a mutsu release. There is no native code here, so no OS-level
patch path applies.

## License

**Artistic-2.0** — declared in `META6.json` and shipped as `LICENSE`.
Vendored verbatim with `LICENSE` / `META6.json` / `README.md` preserved for
attribution, source unmodified (per
[BATTERIES.md §4](../../BATTERIES.md#4-license-policy)).
