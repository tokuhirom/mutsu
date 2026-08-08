# Battery: Digest::HMAC

**Status: bundled and working** (BATTERIES.md rung 2 — the upstream module,
vendored as-is)

## What it is

A generic HMAC (RFC 2104) over any hash function you hand it:

```raku
use Digest::HMAC;
use Digest::SHA2;

hmac-hex("key", "The quick brown fox jumps over the lazy dog", &sha256);
# f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8

my Buf $mac = hmac($key, $data, &sha256);      # raw bytes
```

- `hmac($key, $message, &hash, $blocksize = 64 --> Buf)`
- `hmac-hex($key, $message, &hash, $blocksize = 64 --> Str)`

`$key` and `$message` may be `Str` (encoded as ASCII) or `Blob`. `&hash` must
take and return a `Blob`/`Buf` — the `md5` / `sha1` / `sha256` routines from the
bundled [`Digest`](digest.md) battery and `&sha256` from `OpenSSL::Digest` all
qualify. `$blocksize` defaults to 64, correct for MD5, SHA-1 and SHA-256.

## Why it is bundled

`JSON::JWT` — the token layer under Cro's `Cro::HTTP::Auth::WebToken` — depends
on it directly:

```raku
%pack<signature> = hmac($secret, $sigstring.encode('ascii'), &sha256);
```

Without it, Cro's two `http-auth-webtoken-*` test files could not even load.

Note that this is **not** the same HMAC as the bundled `Digest` distribution's
`HMAC` module. That one is `unit module HMAC` with a named-argument signature
(`hmac(:$key, :$msg, :&hash)`); `Digest::HMAC` is a separate distribution with a
positional signature, and it is the one the ecosystem depends on.

## Which upstream

There are two repositories with this module's name, and they are **not**
interchangeable:

- `raku-community-modules/Digest-HMAC` — v1.0.1, `depends: ["Digest"]`, and its
  test `use`s `Digest` and `Digest::SHA`, neither of which any bundled dist
  provides (`use Digest` fails under stock raku here too). Not vendorable
  against our bundle.
- **`JJ/Raku-Digest-HMAC` — v1.0.7, `zef:jjmerelo`, no runtime dependencies**,
  and its tests `use Digest::MD5` / `Digest::SHA1` / `Digest::SHA2`, all of
  which the bundled `Digest` provides. This is the released version zef installs,
  and the one vendored here.

## Test suite

Both upstream files pass under mutsu against the bundled copy, matching raku:

| File | Covers |
| --- | --- |
| `t/01-basic.t` (6) | the Wikipedia MD5 / SHA-1 / SHA-256 vectors, empty and keyed |
| `t/02-block-size.t` (3) | keys shorter than, equal to and longer than the block size |

Both are in `batteries-whitelist.txt`, so the release gate fetches them fresh at
the pinned commit and runs them against the shipped library. The gate needs
`modules/Digest/lib` on the include path for the hash functions, which is the
`extra_includes` column in `batteries.lock`.

## Provenance and update procedure

Per [BATTERIES.md §3](../../BATTERIES.md#updating-a-vendored-module-must-be-documented-per-library).
To bump the module, re-vendor — do **not** hand-edit the vendored tree:

| Module | Upstream | Pinned version | Commit |
| --- | --- | --- | --- |
| `Digest::HMAC` | <https://github.com/JJ/Raku-Digest-HMAC> | v1.0.7 | `9bae022a` (2023-11-16) |

What is vendored: `lib/` plus `META6.json`, `LICENSE` (MIT), `README.md` for
attribution. Upstream `t/` and CI config are excluded — the release gate fetches
the tests fresh at the pinned commit.

```sh
rsync -a --exclude '.precomp' <checkout>/lib/ modules/Digest-HMAC/lib/
cp <checkout>/{META6.json,LICENSE,README.md} modules/Digest-HMAC/
```

Then bump the `commit` column in `batteries.lock` and re-run
`scripts/battery-testsuite.sh --update`.
