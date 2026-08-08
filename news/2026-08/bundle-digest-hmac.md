# Digest::HMAC is bundled

`Digest::HMAC` joins the bundle as the 23rd library, resolved with zero
configuration like the rest:

```raku
use Digest::HMAC;
use Digest::SHA2;
say hmac-hex("key", "The quick brown fox jumps over the lazy dog", &sha256);
# f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8
```

Both upstream test files pass under mutsu against the bundled copy, matching
raku, and both are in `batteries-whitelist.txt` so the release gate enforces
them. The whitelist diff is exactly the two added lines — nothing else moved.

## Why

`JSON::JWT` depends on it directly (`hmac($secret, $sigstring, &sha256)`), and
`JSON::JWT` is what Cro's `Cro::HTTP::Auth::WebToken` is built on. Without it
Cro's two `http-auth-webtoken-*` test files could not even load — they reported
0 tests.

After bundling:

- `t/http-auth-webtoken-cookie.rakutest` — **2/2, fully green**;
- `t/http-auth-webtoken-bearer.rakutest` — now loads and runs deep into the
  client, and fails on an unrelated, already-recorded bug: `$request.append-header($_)`
  where `$_` is a `Pair` from an array is dispatched as a *named* argument
  (`Cannot resolve caller append-header(Cro::HTTP::Request:D: :Authorization(Str))`).
  That is `todo/deep/pair-namedness-is-a-value-property-not-a-call-site-property.md`,
  not a gap in this battery.

## A correction to the Digest survey

`docs/batteries/digest.md` (the message-digest selection record, 2026-08-04)
rejected `Digest::HMAC` partly because "it states no license anywhere, which
under BATTERIES.md §4 is a hard gate for bundling". **That was wrong.** Upstream
ships an MIT `LICENSE` (`Copyright (c) 2014 Andrew Egeler`); only its
`META6.json` omits a `license` key, which is what an index-based survey sees.
BATTERIES.md §4 asks that the license be permissive, preserved in the vendored
tree and recorded — not that it be declared in the META — so the gate it was
said to fail never applied. The survey has been corrected, and its table row now
reads MIT.

The rest of that entry stands and is worth keeping straight: `Digest::HMAC` does
**not** compete with `Digest` for the message-digest slot, because it supplies
no hash function at all — `hmac($key, $msg, &hash)` takes the digest as a
callback. The two are complementary: `Digest` is the hash functions, and
`Digest::HMAC` is the RFC 2104 construction over any of them.

## Which upstream — the trap

Two repositories carry this module's name and they are **not** interchangeable:

- `raku-community-modules/Digest-HMAC` (v1.0.1) `depends: ["Digest"]`, and its
  test `use`s `Digest` and `Digest::SHA`. Neither is provided by any bundled
  dist — `use Digest` fails under stock raku here too, so that suite cannot run
  against our bundle at all.
- **`JJ/Raku-Digest-HMAC` (v1.0.7, `zef:jjmerelo`, no runtime dependencies)** is
  the version zef actually installs, and its tests `use Digest::MD5` /
  `Digest::SHA1` / `Digest::SHA2` — all provided by the bundled `Digest`.

The first was vendored, found unusable, and replaced. Recording it here because
the name alone does not disambiguate them, and picking the wrong one leads
straight to wanting a gate exclusion that
[`batteries-exclude.txt`](../../batteries-exclude.txt) explicitly forbids ("NOT
a place to park a genuinely failing test").

Note also that this is a different HMAC from the bundled `Digest` distribution's
own `HMAC` module: that one is `unit module HMAC` with a named-argument
signature, while `Digest::HMAC` is positional, and the positional one is what
the ecosystem depends on.

Details: [docs/batteries/digest-hmac.md](../../docs/batteries/digest-hmac.md).
